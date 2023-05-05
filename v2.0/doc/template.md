(:#<comment>#:)		comment

(:_ <code> :)		inject python code
(:@ <name(s)> :)    render parameters

(:? <expr> :)		if expr
(:| <expr> :)		elif expr
(:|:)	     		else
(:?:)	     		endif

(:* <name(s)> : <expr> :)	for name(s) in expr
(:>:)			break
(:<:)			continue
(:|:)	     	else
(:\*:)	        endfor

(:~ <expr> :)		match expr
(:= <expr> :)		case expr
(:=:)	     		default
(:~:)	     		endmatch

(:. <name> :)		with name (a-la modula-2)
(:.:)	     		endwith

(: <expr> :)		evaluate python expression

the template is treated as a sequece of
    <literal> '(:' <directive> ':)' <literal> '(:' <directive> ':)' ...

To let the opening bracket be renderd in the literal part, you can
use '(::'. To let the closing bracket be renderd in the directive
part, you can use '::)'.

