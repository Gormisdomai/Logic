input = "~tv(tn(tvf))"

## EXAMPLE INPUT
#name variables (one char each) ==> pqr
#enter statement ==> pn(q>~r)       
#['p', 'q', 'r'] =
#('t', 't', 't') f
#('t', 't', 'f') t
#('t', 'f', 't') t
#('t', 'f', 'f') t
#('f', 't', 't') f
#('f', 't', 'f') f
#('f', 'f', 't') f
#('f', 'f', 'f') f


def AND(x, y):
	return x and y
def OR(x, y):
	return x or y
def IF(x, y):
	return x <= y
def IFF(x, y):
	return x == y

operators = {"n":AND, "v":OR, ">":IF, "=":IFF}

def debracketize(input):
	
	#find index of first parsable expression
	start = 0
	end = len(input)-1
	for i in range(0, len(input)):
		if input[i] == "(":
			start = i
		elif input[i] == ")":
			end = i
			break

	firsthalf = input[:start]
	secondhalf = input[end+1:]
	contents = input[start+1:end]
	return parse(firsthalf + str(parse(contents)) + secondhalf)

def parse(input):
	if "(" in input: 
		return ((debracketize(input)))
	else: 
		return (evaluateL1(input))

def evaluateL1(input):
	#check for unary operators
	input = list(input)
	
	i = 0
	while i < len(input):
		if (input[i] == "~"):
			input[i+1] = (not boolean(input[i+1]))
			del input[i]
		else:
			i += 1
	if len(input) == 1:
		return input
	return unboolean(operators[input[1]](boolean(input[0]), boolean(input[2])))



def boolean(input):
	return (input == "t" or input == True)
def unboolean(input):
	return "t" if input else "f"

def truthtable(sentences, statement, breadcrumbs):
	if sentences:
		s = sentences[0]
		
		truthtable(sentences[1:], statement.replace(s, "t"), breadcrumbs + ("t",))
		
		
		truthtable(sentences[1:], statement.replace(s, "f"), breadcrumbs + ("f",))
	else:
		print(breadcrumbs),
		print(parse(statement))
		breadcrumbs = ()

if __name__ == "__main__":
	while 1:
		ss = raw_input("name variables (one char each) ==> ")
		st = raw_input("enter statement ==> ")
		print(list(ss)),
		print "="
		truthtable(list(ss), st, ())
