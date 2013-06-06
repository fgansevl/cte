#!/usr/bin/python
''' cte - Configuration Template Engine
    Copyright (c) 2013 Fred Gansevles, cte@gansevles.net

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
'''

import sys
BRACES = ['(:', ':)']

# attrdict - a class that lets a dict be used as an object
class attrdict(dict):
  def __getattr__(self, key):
    try:  return self.__getitem__(key)
    except KeyError, why: raise AttributeError, why
  def __setattr__(self, key, value):
    return self.__setitem__(key, value)
  def __setitem__(self, key, value):
    if isinstance(value, dict) and not isinstance(value, attrdict):
      value = attrdict(value)
    super(attrdict, self).__setitem__(key, value)
  def update(self, other):
    for key, value in other.items():
      self.__setitem__(key, value)

class stdout(object):
  def __init__(self, write):
    self._ = write
  def write(self, arg):
    return self._(arg)

# Scope - a class that mimics a dict with modula-2 like 'with' support
# and has execute and evaluate methods

from contextlib import contextmanager
class Scope(object):
  def __init__(self, *args, **kwds):
    from collections import deque
    self.__ = {} # global scope for evaluate and execute
    self.__scopes = deque([attrdict(*args, **kwds), self.__, __builtins__])
  @contextmanager
  def __call__(self, scope = None):
    self.__scopes.appendleft(attrdict(self[scope] or ()))
    yield
    self.__scopes.popleft()
  def __getitem__(self, key):
    value = None
    if key is not None:
      parts = iter(key.split('.'))
      part = parts.next()
      for scope in self.__scopes:
        if part in scope: break
      while part in scope:
        try: next = parts.next()
        except StopIteration:
          value = scope[part]
          break
        else:
          scope = scope[part]
	  part = next
    return value
  def __setitem__(self, key, value):
    if key is None: return
    scope = self.__scopes[0]
    parts = iter(key.split('.'))
    part = parts.next()
    while part in scope:
      try: next = parts.next()
      except StopIteration:
        if value is None: del scope[part]
        else: scope[part] = value
        break
      else:
        scope = scope[part]
        part = next
    else:
      if value is not None:
        try: parts.next()
        except StopIteration: scope[part] = value
        else:
          scope[part] = attrdict()
          self[key] = value
  def update(self, other):
    scope = self.__scopes[0]
    scope.update(other)
  def evaluate(self, tag):
    result = eval(tag, self.__, self)
    if result is None: result = ''
    return result
  def execute(self, tag, stdout):
    sys_stdout = sys.stdout
    sys.stdout = stdout
    try:
      exec tag in self.__, self
    finally:
      sys.stdout = sys_stdout
  def system(self, tag):
    from subprocess import check_output, CalledProcessError
    try: return check_output(self.evaluate(tag), shell = True)
    except CalledProcessError: return ''

def iterate(template):
  # generate a list of [literal, directive, literal, directive, ...] 
  # where even elements are directive - stuff inside <BRACES>
  # and odd elements are literals
  start = 0
  side = 0
  # a brace is escaped by duplicating the 'inner' character
  escaped = [BRACES[0]+BRACES[0][-1], BRACES[-1][0]+BRACES[-1]]
  def is_escaped(pos):
    # i.e, (:: and ::) are the escaped BRACES
    return template[pos] == BRACES[side][abs(side)-1]
  done = False
  strip_nl = False
  while not done:
    brace = BRACES[side]
    begin = start
    value = ''
    while True:
      end = template.find(brace, begin)
      if end < 0:
        # no more BRACES
        part = template[begin:]
        end = len(template) - len(brace)
        done = True
        break
      else: part = template[begin:end]
      if side: pos = end-1
      else:
        pos = end+len(brace)
	if strip_nl and part.startswith('\n'):
	  # suppress trailing line break
	  part = part[1:]
	  strip_nl = False
      for i, escape in enumerate(escaped):
	if escape in part: part = part.replace(escape, BRACES[i])
      if is_escaped(pos):
	if side:
	  part = part[:-1]
	  pos += len(brace)
	part = part + brace
	begin = pos+1
	value = ''.join((value, part))
      else: break
    if side: # directive
      if part.startswith('-'):
        part = part[1:]
      if part.startswith('#'):
	# remove comments
	value = None
	if part.startswith('##'): # multi-line
	  # locate the end tag, i.e. part.join(BRACES)
	  tag = part
	  # look for 4 possible end tags
	  # (:##:), (:-##:), (:##-), (:-##-:)
	  for part in tag, ('-' + tag), (tag + '-'), ('-' + tag + '-'):
	    pos = template.find(part.join(BRACES), end)
	    if pos >= 0:
	      break
	  else:
	    # no end comment tag
	    done = True
	    break
	  end = pos + len(tag) + len(brace)
      if part.endswith('-'):
	part = part[:-1]
	# remember to suppress trailing line break
	strip_nl = True
    else: # literal
      if not done and template[begin+len(brace)] == '-':
	# suppress leading whitespace
	part = part.rstrip('\t ')
	end += 1
    if not value is None:
      value = ''.join((value, part))
    start = end + len(brace)
    yield value
    side = 1 - side # switch BRACES

def generate(initial, pieces):
  # indents
  T = '  '
  TT = T+T
  TTT = TT+T
  TTTT = TT+TT
  STACK = []
  CODE = []
  n = 1	# loop index
  has_code = [False]

  # helper functions
  def O(text):
    has_code[-1] = True
    return '\n'.join('%s%s' % (''.join(STACK), line)
		     for line in text.splitlines())
  def W(text):
    has_code[-1] = True
    return '%s_(%r)' % (''.join(STACK), text)
  def E(text):
    has_code[-1] = True
    return '%s_(str(%s))' % (''.join(STACK), text)
  def indent(num = 1):
    has_code.append(False)
    CODE.append([])
    STACK.append(T * num)
  def dedent():
    code = CODE.pop()
    if has_code.pop() or code:
	for line in code:
	  if isinstance(line, int):
	    STACK[-1] = STACK[-1][:line*len(T)]
	  else:
	    yield line
    else:
      yield 'pass'
    STACK.pop()
  def tag_split(d, n = 1, space = True, strip = True):
    if space:
      assert d.endswith((' ', '\n')), repr(d)
      d = d.rstrip()
    tag, line = d.split(None, 1)
    assert len(tag) == n, repr(d)
    if strip: line = line.lstrip()
    return tag, line

  # first, generate the initial code
  yield O('#!/usr/bin/python')
  yield O('import sys')
  yield O('import cte')

  # now, generate the 'render' function
  yield O('def render(settings = {}):')
  indent()
  yield O('data = %r' % initial)
  yield O('data.update(settings)')
  yield O('class _B(Exception): pass # break')
  yield O('class _C(Exception): pass # continue')
  yield O('output = []')
  yield O('_ = output.append')
  yield O('_o = cte.stdout(_)')
  yield O('_s = cte.Scope(data)')
  yield O('try:')
  yield O(T+'with _s():')
  indent(2)
  while True:
    try:
      literal = pieces.next()
      # emit the literal
      if literal: yield W(literal)

      directive = pieces.next()
      if not directive or not directive.strip():
	# comment (None) or (: :)
	if directive is not None:
	  for tail in dedent():
	    yield O(tail)
      elif directive.startswith('='):
        # (:= <expr> :)
        tag, expr = tag_split(directive)
        yield E('_s.evaluate(%r)' % expr)
      elif directive.startswith('!'):
        # (:! <name>=<command> :)
        tag, line = tag_split(directive)
        name, command = [value.strip() for value in line.split('=', 1)]
        yield O('_s[%r] = _s.system(%r)' % (name, command))
      elif directive.startswith('_'):
	if directive.strip() == '_':
	  # (:_:) = break
	  yield O('raise _B')
	elif directive.strip() == '__':
	  # (:__:) = continue
	  yield O('raise _C')
	else:
	  # (:_ <name>=<expr> :)
	  tag, line = tag_split(directive)
	  name, expr = [value.strip() for value in line.split('=', 1)]
	  yield O('items_%d = _s.evaluate(%r)' % (n, expr))
	  yield O('if items_%d:' % n)
	  yield O(T+'try:')
	  yield O(TT+'loop_%d = iter(items_%d)' % (n, n))
	  yield O(TT+'item_%d = loop_%d.next()' % (n, n))
	  yield O(T+'except (TypeError, StopIteration): items_%d = None' % n)
	  yield O('if items_%d:' % n)
	  yield O(T+'with _s():')
	  yield O(TT+'_i_%d = dict(first = True, last = False, index = 0)' % n)
	  yield O(TT+'_s["_"] = _i_%d' % n)
	  yield O(TT+'while not _i_%d["last"]:' % n)
	  yield O(TTT+'try:')
	  yield O(TTTT+'next_item_%d = loop_%d.next()' % (n, n))
	  yield O(TTT+'except StopIteration:')
	  yield O(TTTT+'_i_%d["last"] = True' % n)
	  yield O(TTTT+'next_item_%d = None' % n)
	  yield O(TTT+'_s.execute("""%s = %%r""" %% item_%d, _o)' % (name, n))
	  yield O(TTT+'try:')
	  indent(4)
	  CODE[-1].extend([-1, # dedent 1 T
			  'except _B: break',
			  'except _C: pass',
			  '_i_%d["first"] = False' % n,
			  '_i_%d["index"] += 1' % n,
			  'item_%d = next_item_%d' % (n, n)])
	  n += 1
      elif directive.startswith('.'):
        # (:. <name> :)
        tag, name = tag_split(directive)
        yield O('with _s(%r):' % name)
        indent()
      elif directive.startswith('?'):
        # (:? <expr> :)
        tag, expr = tag_split(directive)
        yield O('if _s.evaluate(%r):' % expr)
        indent()
      elif directive.startswith('|?'):
        # (:|? <expr> :)
        tag, expr = tag_split(directive, 2)
        for tail in dedent():
	  yield O(tail)
        yield O('elif _s.evaluate(%r):' % expr)
        indent()
      elif directive.strip() == '|':
        # (:|:)
        for tail in dedent():
	  yield O(tail)
        yield O('else:')
        indent()
      else:
        # (: <code> :)
        assert directive.startswith((' ', '\n')), repr(directive)
        assert directive.endswith((' ','\n')), repr(directive)
	code = directive.strip()
	yield O('_s.execute(%r, _o)' % code)
    except StopIteration:
      # we're done
      break

  for tail in dedent():
    yield O(tail)
  yield O('except (_B, _C): pass')
  yield O('return "".join(output)')
  for tail in dedent():
    yield O(tail)

  # finally, generate the script startup code
  yield O('if __name__ == "__main__":')
  yield O(T+'d = cte.parse()')
  yield O(T+'print render(d)')

def parse(args = None, usage = ''):
  from optparse import OptionParser
  parser = OptionParser(usage = '%prog ' + usage + ' [options] key=val ...')
  parser.add_option("-i", "--ini", action = "store",
		    help="load settings from INI file")
  parser.add_option("-j", "--json", action = "store",
		    help="load settings from JSON file")
  parser.add_option("-y", "--yaml", action = "store",
		    help="load settings from YAML file")

  (options, args) = parser.parse_args(args)
  d = {}
  if options.ini:
    from ConfigParser import ConfigParser
    conf = ConfigParser()
    if options.ini == '-':
      cfg = sys.stdin
    else:
      cfg = file(options.ini)
    conf.readfp(cfg)
    d.update(conf.defaults())
    for sect in conf.sections():
      d[sect] = dict(conf.items(sect))
  if options.json:
    import json
    if options.json == '-':
      cfg = sys.stdin
    else:
      cfg = file(options.json)
    d.update(json.load(cfg))
  if options.yaml:
    import yaml
    if options.yaml == '-':
      cfg = sys.stdin
    else:
      cfg = file(options.yaml)
    d.update(yaml.load(cfg))
  for arg in args:
    key, val = [s.strip() for s in arg.split('=', 1)]
    d[key] = val
  return d

# TODO: pass extra args to parse for extending the options
# ?also return the xtra args?
if __name__ == '__main__':
  if len(sys.argv) == 1:
    sys.argv.append('-h')
  if '-h' in sys.argv:
    parse(sys.argv, 'template')
  else:
    template = file(sys.argv[1]).read()
    d = parse(sys.argv[2:], 'template')
  print '\n'.join(generate(d, iterate(template)))

