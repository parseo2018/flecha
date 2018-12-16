import sys, os
sys.path.insert(0,"../..")

from lexer_flecha import Flecha

if __name__ == "__main__":

    if len(sys.argv) != 2:
        sys.stderr.write('Uso: %s input.flecha\n' % (sys.argv[0],))
        sys.exit(1)

    flecha = Flecha()

    filename = sys.argv[1]
    f = open(filename, 'r')
    data = f.read()
    f.close()

    program = flecha.yacc.parse(data)
    print(program)