#!/usr/bin/python3
import ast

BRACES = ["(:", ":)"]

DIRECTIVES = {
    ("_", " "): "exec",      # (:_ <code> :)
    ("@", " "): "def",       # (:@ <name(s)> :)

    ("?", " "): "if",        # (:? <expr> :)
    ("|", " "): "elif",      # (:| <expr> :)
    ("|", "") : "else",      # (:|:)
    ("?", "") : "endif",     # (:?:)

    ("*", " "): "for",       # (:* <name(s)> : <expr> :)
    (">", "") : "break",     # (:>:)
    ("<", "") : "continue",  # (:<:)
    ("*", "") : "endfor",    # (:*:)

    ("~", " "): "match",     # (:~ <expr> :)
    ("=", " "): "case",      # (:= <expr> :)
    ("=", "") : "default",   # (:=:)
    ("~", "") : "endmatch",  # (:~:)

    (".", " "): "with",      # (:. <name(s)> :)
    (".", "") : "endwith",   # (:.:)

    ("", " ") : "eval",      # (: <expr> :)
}


class __:
    """
    This class contains code fragments:
      - _ is only used by the generated render function
        and contains the namespace handling code
      - main and context contain code that is used by this
        script (cte.py) and by the generated template script
        Here the commandline parsing is implemented
      - merge and output is code only used by the generate script
      - template and generate is code only used by this script
    """

    _ = """
# only use '_' to avoid namespace  clutter
# '_' is removed from the global scope in __init__

class _:
    def __init__(self, scope):
        # cleanup the global namespace
        scope.pop("_")
        self.__scope = [scope.copy()]

    @property
    def __dict(self):
        return self.__scope[-1]

    def __getitem__(self, item):
        value = self.__dict.get(item)
        if value is None and "_" in item:
            value = self.__dict.get(item.replace("_", "-"))
        return value

    def __setitem__(self, item, value):
        if value is None:
            value = ""
        self.__dict[item] = value

    from contextlib import contextmanager as _

    @_
    def __call__(self, scopes=(), match=False):
        self.__scope.append(self.__dict.copy())
        if match:
            values = []
        if not isinstance(scopes, tuple):
            scopes = (scopes,)
        for scope in scopes:
            if scope is None:
                continue
            if match:
                if not isinstance(scope, str):
                    raise SyntaxError(f"str expected, got {scope}")
                values.append(self[scope])
            else:
                if not isinstance(scope, dict):
                    raise SyntaxError(f"dict expected, got {scope}")
                self.__dict.update(scope)
        if match:
            yield tuple(values)
        else:
            yield
        self.__scope.pop()

_ = _(globals())
"""

    main = """
def __main__():
    import sys
    import argparse

    # context parsers
    from configparser import ConfigParser
    import json

    parser = argparse.ArgumentParser(epilog="if a file is expected, '-' reads from stdin")
    parser.add_argument("-o", "--output", default="-", help="write output to file")
    parser.add_argument(
        "-e", "--eval", help="eval the key=value args", metavar="key=value", nargs="+"
    )
    parser.add_argument("-i", "--ini", help="load settings from INI file")
    parser.add_argument("-j", "--json", help="load settings from JSON file")

    # add-on context parsers
    try:
        import yaml
        parser.add_argument("-y", "--yaml", help="load settings from YAML file")
        has_yaml = True
    except ImportError:
        has_yaml = False
    try:
        import nestedtext
        parser.add_argument("-n", "--nestedtext", help="load settings from NestedText file")
        has_nestedtext = True
    except ImportError:
        has_nestedtext = False
"""

    context = """
    context = {}

    if args.eval:
        kwds = dict()
        for arg in args.eval:
            key, val = [s.strip() for s in arg.split("=", 1)]
            kwds[key] = eval(val)
        update(context, kwds)

    if args.ini:
        conf = ConfigParser()
        if args.ini == "-":
            cfg = sys.stdin
        else:
            cfg = open(args.ini)
        conf.readfp(cfg)
        update(context, conf.defaults())
        for sect in conf.sections():
            context[sect] = dict(conf.items(sect))

    if args.json:
        if args.json == "-":
            cfg = sys.stdin
        else:
            cfg = open(args.json)
        update(context, json.load(cfg))

    if has_yaml and args.yaml:
        if args.yaml == "-":
            cfg = sys.stdin
        else:
            cfg = open(args.yaml)
        update(context, yaml.safe_load(cfg))

    if has_nestedtext and args.nestedtext:
        if args.nestedtext == "-":
            cfg = sys.stdin
        else:
            cfg = open(args.nestedtext)
        update(context, nestedtext.load(cfg))

    if args.output == "-":
        output = sys.stdout
    else:
        output = open(args.output, "w")
"""

    merge = """
    parser.add_argument("-m", "--merge", action="store_true",
                        help="merge (not update) the settings with the default context")
    parser.add_argument("-d", "--defaults", action="store_true",
                        help="show the default context")
    args = parser.parse_args()

    if args.merge:
        def update(this, other):
            if isinstance(this, dict) and isinstance(other, dict):
                for key in other:
                    if key in this:
                        this[key] = update(this[key], other[key])
                    else:
                        this[key] = other[key]
                return this
            if isinstance(this, list) and isinstance(other, list):
                this = list(this)
                for value in other:
                    if value not in this:
                        this.append(value)
                return this
            return other
    else:
        update = dict.update
"""

    output = """
    if args.defaults:
        import pprint
        pprint.pprint(context)
    else:
        for line in __(context):
            output.write(line)

if __name__ == "__main__":
    __main__()
"""

    template = """
    parser.add_argument("-s", "--script", action="store_true", help="write code to file")
    parser.add_argument("template", help="the '.ct' template to compile ('-' reads from stdin)")
    args = parser.parse_args()
    update = dict.update
"""

    generate = """
    if args.template == "-":
        template = sys.stdin
    else:
        template = open(args.template)

    source = generate(context, template.read())

    if args.script:
        output.write(source)
        output.write(__._)
        output.write(__.main)
        output.write(__.merge)
        output.write(__.context.format(context,))
        output.write(__.output)
        if args.output != "-":
            import os
            os.chmod(args.output, 0o755)
    else:
        ns = {}
        exec(source + __._, ns)
        for line in ns["__"](context):
            output.write(line)
"""


class ParseException(Exception):
    pass




def targets(node):
    class Node(ast.NodeVisitor):
        def visit_comprehension(self, node):
            targets.add(node.target.id)

    targets = set()
    Node().visit(node)
    return targets


class Convert(ast.NodeTransformer):
    def __init__(self, targets):
        super().__init__()
        self.targets = targets

    def visit_Call(self, node):
        func = node.func
        if isinstance(func, ast.Attribute):
            func = ast.Attribute(
                value=self.visit(node.func.value),
                attr=node.func.attr,
                ctx=node.func.ctx,
            )
        if isinstance(func, ast.Subscript):
            func = ast.Subscript(
                value=node.func.value,
                slice=self.visit(node.func.slice),
                ctx=node.func.ctx,
            )
        return ast.Call(
            func=func,
            args=[self.visit(arg) for arg in node.args],
            keywords=[self.visit(kwd) for kwd in node.keywords],
        )

    def visit_Attribute(self, node):
        if (
            isinstance(node.value, ast.Name)
            and node.value.id in self.targets
        ):
            return node
        return self.visit(
            ast.Subscript(
                value=node.value,
                slice=ast.Constant(value=node.attr),
                ctx=node.ctx,
            ),
        )

    def visit_Name(self, node):
        if node.id in self.targets:
            return node
        return ast.Subscript(
            value=ast.Name(id="_", ctx=node.ctx),
            slice=ast.Constant(value=node.id),
            ctx=node.ctx,
        )


class Loop(Convert):
    def visit_arguments(self, node):
        return ast.arguments(
            posonlyargs=node.posonlyargs,
            args=[self.visit(ast.Name(id=arg.arg, ctx=ast.Load())) for arg in node.args],
            kwonlyargs=node.kwonlyargs,
            kw_defaults=node.kw_defaults,
            defaults=node.defaults
        )


class Case(ast.NodeVisitor):
    def __init__(self, match):
        super().__init__()
        self.match = match
        self.case = set()

    def visit_Name(self, node):
        self.match.add(node.id)

    def visit_MatchSequence(self, node):
        for pattern in node.patterns:
            self.visit(pattern)

    def visit_MatchAs(self, node):
        if node.pattern:
            self.case.add(node.name)
            self.visit(node.pattern)
        else:
            if node.name:
                self.match.add(node.name)

class Output:
    quotes = '"""'

    def __init__(self):
        self.lines = []

    @staticmethod
    def partition(line, sep):
        head, sep, tail = line.partition(sep)
        # if head ends with an odd number of backslashes
        # then the sep is escaped
        while sep and (len(head) - len(head.rstrip("\\"))) % 2 == 1:
            head2, sep2, tail = tail.partition(sep)
            head = sep.join((head, head2))
            sep = sep2
        return head, sep, tail

    def add(self, line):
        # add a literal to the output.
        # the code that is generated to output this text
        # (by Code.output()) will be:
        #    r"""{line}"""
        # this means that <literal> may contain any text,
        # except '"""'
        #
        # split the line in <text-with-no-"""> <"""> <text-with-no-""">
        # and signal Code.output() which quote to use (i.e. ' or """)
        # and with what prefix (i.e. "r" or "")
        head, sep, tail = self.partition(line, self.quotes)
        while sep:
            self.lines.append(("r", self.quotes, head))
            self.lines.append(("", "'", self.quotes))
            head, sep, tail = self.partition(tail, self.quotes)
        self.lines.append(("r", self.quotes, head))

    def set(self, expr):
        self.lines.append(("rf", self.quotes, expr))

    def get(self):
        return self.lines


class Generate:
    def __init__(self, context={}):
        self.context = context
        self.code = Code()

    def parse(self, iterator):
        directive = None
        strip_tail = False
        strip_lead = False
        output = Output()
        lead, middle, tail = next(iterator)
        for tag, sep, data in iterator:
            if directive == "eval" and not strip_lead:
                if lead:
                    output.add(lead)
            if self.code.in_template and middle:
                output.add(middle)
            # handle (:- and -:) for "eval"'s lead/tail stripping
            if tag == "-":
                tag = ""
                strip_tail = True
            else:
                strip_tail = False
            if tag == "" and data.endswith("-"):
                data = data[:-1]
                strip_lead = True
            else:
                strip_lead = False
            expr = data.strip()
            directive = DIRECTIVES.get((tag, sep))
            if directive == "eval" and not strip_tail:
                if tail:
                    output.add(tail)
            try:
                if directive is None:
                    raise ParseException("Unknown")
                if not self.code.allow(directive):
                    raise ParseException("Unexpected")
                if directive == "eval":
                    output.set(expr)
                else:
                    self.code.output(output.get())
                    output = Output()
                    self.code(directive, expr)
            except ParseException as err:
                (why,) = err.args
                if isinstance(why, set):
                    raise SyntaxError(f"{why} is protected")
                raise SyntaxError(
                    f"{why} directive "
                    f"{''.join((tag, sep, data, sep)).join(BRACES)!r}"
                )
            lead, middle, tail = next(iterator)
        else:
            if self.code.in_template and middle:
                output.add(middle)
            if directive == "eval" and not strip_tail:
                if tail:
                    output.add(tail)
        self.code.output(output.get())
        self.code.end(self.context)
        return "\n".join(self.code.lines)



class Code:
    def __init__(self):
        self.depth = 0  # indent
        self.caller = []
        self.allowed = [{"exec", "def"}]
        self.__lines = [["#!/usr/bin/env python3\n"]]
        self.in_template = False
        self.match_case = []
        self.case = set()  # set by case_expr

    @property
    def lines(self):
        return self.__lines[-1]

    def __call__(self, directive, arg):
        if directive.startswith("end"):
            top = self.caller.pop()
            if directive != f"end{top}":
                raise ParseException("Illegal")
            self.dedent()
            self.allowed.pop()
            if directive == "endmatch":
                self.dedent()  # to compensate for "case" indent
                self.dedent()  # to prepare for "with" indent
                # now, create the with _(...) as ... scope
                arg = tuple(self.match_case.pop())
                lines = self.__lines.pop()
                #self.dedent()  # to compensate for correct indentation
                self.scope(arg)  # does self.indent()
                # emit the suspended code
                self.lines.extend(lines)
                # and close the with-scope
                self.dedent()  # to compensate for "with" indent
            return

        allowed = self.allowed[-1]
        if directive in ("def", "if", "for", "with", "match"):
            allowed = {"eval", "for", "if", "with", "match"}
            self.allowed.append(allowed)
            self.caller.append(directive)
            if directive != "def":
                allowed.add(f"end{directive}")
                # supend the output until the match-scope is known
            if directive == "match":
                self.match_case.append(set())
                # suspend output until endmatch but let the
                self.__lines.append([])
                # submitted code have the correct indentation
                self.indent()

        match directive:
            case "break":
                self.emit("break")
            case "case":
                # gather the use.match parameters
                self.dedent()
                self.condition("case", arg)  # does self.indent()
                # inject the use.case parameters
                for case in self.case:
                    self.emit(f"_[{case!r}] = {case})")
            case "continue":
                self.emit("continue")
            case "def":
                self.define(arg)  # does self.indent()
                self.in_template = True
            case "default":
                allowed.remove("case")
                allowed.remove("default")
                self.dedent()
                self.emit("case _:")
                self.indent()
            case "elif":  # or case
                self.dedent()
                self.condition("elif", arg)  # does self.indent()
            case "else":  # or default
                caller = self.caller[-1]
                if caller == "if":
                    allowed.remove("elif")
                allowed.remove("else")
                self.dedent()
                self.emit("else:")
                self.indent()
            case "exec":
                self.execute(arg)
            case "for":
                allowed.add("else")
                self.loop(arg)  # does self.indent()
            case "if":
                allowed.add("elif")
                allowed.add("else")
                # when the last caller is 'for',
                # ignoring other 'if', then
                # 'break' and 'continue' are allowed
                callers = list(reversed(self.caller))
                while callers.pop() == "if":
                    continue
                if callers.pop() == "for":
                    allowed.add("break")
                    allowed.add("continue")
                self.condition("if", arg)  # does self.indent()
            case "match":
                allowed.add("case")
                allowed.add("default")
                self.condition("match", arg)  # does self.indent()
                self.indent()  # to compensate for "case" dedent
            case "with":
                self.scope(arg)  # does self.indent()

    # output control

    def emit(self, line):
        # emit raw code
        self.lines.append(f"{'  ' * self.depth}{line}")

    def output(self, lines):
        for pfix, quote, value in lines:
            if value:
                if "f" in pfix:
                    value = f"{{{self.expr(value)}}}"
                self.emit(f"yield {pfix}{quote}{value}{quote}")

    def indent(self):
        self.depth += 1

    def dedent(self):
        self.depth -= 1

    # helpers

    def allow(self, directive):
        return directive in self.allowed[-1]

    @staticmethod
    def __unparse(tree):
        # make sure ast.uprase avoids backslashes globally
        return ast._Unparser(_avoid_backslashes=True).visit(tree)

    def __eval(self, expr):
        expr = expr.strip()
        if "\n" in expr or not expr:
            expr = f"({expr})"
        try:
            tree = ast.parse(expr, "", "eval")
            return self.__unparse(tree), tree
        except SyntaxError as err:
            raise SyntaxError(f"in {BRACES[0]} {expr} {BRACES[1]}")

    def __expr(self, arg):
        return self.__eval(arg)[0]

    def expr(self, line):
        expr, tree = self.__eval(line)
        try:
            convert = Convert(targets(tree))
            convert.visit(tree)
        except Exception as err:
            raise ParseException(str(err))

        return self.__unparse(tree)

    def case_expr(self, line):
        empty_match = f"""
match None:
    case {line}:
        ...
"""
        tree = ast.parse(empty_match, "", "exec")
        case = Case(self.match_case[-1])
        case.visit(tree)
        self.case = case.case

        parts = [self.__unparse(case.pattern)]
        if case.guard:
            parts.append(self.__unparse(case.guard))
        return " if ".join(parts)

    def loop_expr(self, expr):
        args, _, body = expr.partition(":")
        if _ != ":":
            raise ParseException("Invalid")
        lambda_loop = f"lambda {args}: ({body})"
        tree = ast.parse(lambda_loop, "", "eval")

        convert = Loop(targets(tree))
        convert.visit(tree)
        return self.__unparse(tree.body.args), self.__unparse(tree.body.body)

    def def_expr(self, arg):
        expr, tree = self.__eval(arg)
        body = tree.body
        if type(body) == ast.Name:
            return [body.id]
        if type(body) == ast.Tuple and set(map(type, body.elts)) == {ast.Name}:
            return [el.id for el in body.elts]
        elif arg:
            raise SyntaxError(arg)
        return []

    # directives

    def condition(self, name, expr):
        expr = self.__expr(expr)
        self.emit(f"{name} {self.expr(expr)}:")
        self.indent()

    def define(self, arg):
        names = self.def_expr(arg)

        self.emit("# Render function")
        self.emit("def __(__):")
        self.indent()
        self.emit("# cleanup the global namespace")
        self.emit("_ = globals().pop('_')")
        for name in names:
            self.emit(f"_[{name!r}] = __.get({name!r})")
        self.emit("globals().pop('__')")

    def end(self, context):
        while self.caller:
            self.dedent()
            caller = self.caller.pop()
            if caller != "def":
                print(f"# Warning: missing (: end{caller} :)")

    def execute(self, expr):
        expr = expr.rstrip()
        # check for '_' and '__'
        ns = {}
        exec(compile(expr, "_", "exec"), ns)
        illegal = set()
        for protected in ("_", "__"):
            if protected in ns:
                illegal.add(protected)
        if illegal:
            raise ParseException(illegal)
        self.emit(expr)

    def loop(self, arg):
        name, expr = self.loop_expr(arg)

        self.emit(f"for {name} in {expr}:")
        self.indent()

    def scope(self, arg):
        if isinstance(arg, tuple):
            # match
            if arg:
                self.emit(f"with _({arg!r}) as {','.join(arg)}:  # match scope")
            else:
                self.emit(f"with _():  # match scope")
        else:
            expr, tree = self.__eval(arg)
            if type(tree.body) not in (ast.Call, ast.Name, ast.Tuple):
                raise ParseException("Untrusted")
            self.emit(f"with _({self.expr(expr)}):  # with scope")
        self.indent()


def iterate(template):
    # generate a list of [literal, directive, literal, directive, ...]
    # where even elements are directive - stuff inside <BRACES>
    # and odd elements are literals

    # first, remove the multiline comments
    # that is (:#<comment>#:)
    parts = []
    comments = [f"{BRACES[0]}#", f"#{BRACES[1]}"]

    part, sep, template = template.partition(comments[0])
    while sep:
        parts.append(part)
        _, sep, template = template.partition(comments[1])
        if not sep:
            raise SyntaxError(f"no closing {comments[1]!r}")
        part, sep, template = template.partition(comments[0])
    else:
        parts.append(part)
    template = "".join(parts)

    while True:
        # literal part
        brace = BRACES[0]
        text, sep, template = template.partition(brace)
        # check if this is an escaped brace
        # a brace is escaped by duplicating the 'inner' character
        # i.e. '(::'
        while template.startswith(brace[1]):
            # replace the escaped brace and continue
            escaped = text
            text, sep, template = template[1:].partition(brace)
            text = brace.join((escaped, text))
        # lead, middle, tail
        # lead is every space including the first newline
        lead, nl, middle = text.partition("\n")
        if not lead or lead.isspace():
            lead = lead + nl
        else:
            # not only spaces --> no lead
            middle = text
            lead = ""
        text = middle
        middle, nl, tail = text.rpartition("\n")
        if tail.isspace():
            middle = middle + nl
        else:
            # not only spaces --> no tail
            middle = text
            tail = ""
        yield lead, middle, tail
        if sep != brace:
            # no more braces
            return
        # directive part
        brace = BRACES[1]
        text, sep, template = template.partition(brace)
        # check if this is an escaped brace
        # a brace is escaped by duplicating the 'inner' character
        # i.e. '::)'
        while text.endswith(brace[0]) and sep == brace:
            # replace the escaped brace and continue
            escaped = text[:-1]
            text, sep, template = template.partition(brace)
            text = brace.join((escaped, text))
        if sep != brace:
            raise SyntaxError(f"no closing {brace!r}")
        if text.startswith("\n"):
            # treat a leading "\n" as " "
            text = text.replace("\n", " ", 1)
        if text.startswith(" "):
            yield text.partition(" ")
        else:
            # in-directive comment
            if text[1:].startswith("\n"):
                # treat a leading "\n" as " "
                text = text.replace("\n", " ", 1)
            elif text[1:].startswith("#"):
                # if the first character after the directive token
                # is a '#' then only the directive token is used
                text = text[0]
            yield text.partition(" ")


def generate(context, template):
    gen = Generate(context)
    return gen.parse(iterate(template))


if __name__ == "__main__":
    exec(__.main +
         __.template +
         __.context +
         __.generate
    )

    __main__()
