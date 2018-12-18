import sys, os
sys.path.insert(0,"../..")

from lexer_flecha import Flecha

if __name__ == "__main__":

    if len(sys.argv) != 2:
        sys.stderr.write('Error de uso. Ejemplo: %s archivo_input: \n' % (sys.argv[0],))
        sys.exit(1)

    flecha = Flecha()

    filename = sys.argv[1]
    f = open(filename, 'r')
    data = f.read()
    f.close()

    bare_filename = filename.replace('.input', '') 
    flecha_filename = bare_filename + '.json'

    program = flecha.yacc.parse(data)

    f = open(flecha_filename, 'w')
    f.write(str(program))
    f.close()