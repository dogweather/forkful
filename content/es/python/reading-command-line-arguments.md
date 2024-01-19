---
title:                "Leyendo argumentos de la línea de comandos"
html_title:           "Bash: Leyendo argumentos de la línea de comandos"
simple_title:         "Leyendo argumentos de la línea de comandos"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Los argumentos de la línea de comandos en Python son parámetros específicos que se pasan durante la ejecución de un programa. Los programadores utilizamos esto para permitir que nuestro software actúe de manera variable y dinámica en base a la entrada del usuario.

## Cómo hacerlo:

Aquí tienes un ejemplo de cómo leer argumentos desde la línea de comandos usando el módulo `sys` de Python:

```Python
import sys

def main(args):
    print('Número de argumentos:', len(args), 'argumentos.')
    print('Lista de argumentos:', args)

if __name__ == "__main__":
    main(sys.argv)
```

Cuando ejecutes este código desde la línea de comandos de esta manera: `python test.py arg1 arg2`, obtendrás este resultado:

```Python
Número de argumentos: 3 argumentos.
Lista de argumentos: ['test.py', 'arg1', 'arg2']
```

La lista args incluye todos los argumentos de la línea de comandos, siendo el primero (índice 0) el nombre del script ejecutado.

## Buceo Profundo:

Históricamente, `sys.argv` ha sido utilizado como el estándar para la lectura de los argumentos de línea de comandos en Python desde sus primeras versiones. Pero existen alternativas como el módulo `argparse` que proporciona opciones más versátiles y complejas, como la posibilidad de añadir descripciones a los argumentos o utilizar diferentes tipos de datos.

Cuando Python recibe argumentos de la línea de comandos, los recoge en la estructura `argv`, que es una lista de tipo `string`. El primer item es siempre el nombre del script ejecutado, seguido de cualquier argumento que se haya pasado en el orden en que se han proporcionado.

## Mira También:

1. Módulo sys de Python: [https://docs.python.org/3/library/sys.html](https://docs.python.org/3/library/sys.html)
2. Módulo argparse de Python: [https://docs.python.org/3/library/argparse.html](https://docs.python.org/3/library/argparse.html)
3. Guía detallada para usar argparse: [https://realpython.com/command-line-interfaces-python-argparse/](https://realpython.com/command-line-interfaces-python-argparse/)
4. Argumentos de línea de comandos en Python: [https://www.tutorialspoint.com/python/python_command_line_arguments.htm](https://www.tutorialspoint.com/python/python_command_line_arguments.htm)