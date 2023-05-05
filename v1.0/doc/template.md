(:##:)			multiline comment (upto next ##)
(:# comment :)		comment

(: <code> :)		execute python code
(:= <expr> :)		evaluate python expression
(:! <name>=<command> :)	set name to output of command

(:_ <name>=<expr> :)	for name in expr
(:_:)			break
(:__:)			continue

(:? <expr> :)		if expr
(:|? <expr> :)		elif expr

(:. <name> :)		with name (a-la modula-2)

(:|:)			else (for/if)
(: :)			end (for/if/with)
