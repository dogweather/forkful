---
title:                "Python: Leyendo argumentos de línea de comando."
simple_title:         "Leyendo argumentos de línea de comando."
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué deberías leer argumentos de línea de comandos?

Siempre que escribimos un programa en Python, podemos correrlo desde la línea de comandos. Esta es una forma eficiente de ejecutar nuestro código y nos permite pasarle información adicional al programa. Leer y entender los argumentos de línea de comandos es fundamental para cualquier programador.

## Cómo leer argumentos de línea de comandos en Python

Para leer los argumentos de línea de comandos en Python, usamos el módulo `sys`. Lo importamos al principio del programa:

```Python
import sys
```

Luego, podemos acceder a los argumentos usando la lista `sys.argv`. Esta lista contendrá todos los argumentos pasados al programa, incluyendo el nombre del archivo Python que estamos ejecutando. Por ejemplo, si ejecutamos el siguiente comando en la línea de comandos:

```
python programa.py arg1 arg2 arg3
```

La lista `sys.argv` tendrá el siguiente contenido:

``` 
['programa.py', 'arg1', 'arg2', 'arg3']
```

Podemos acceder a cada argumento individualmente como si fuera un elemento de la lista. Por ejemplo, para imprimir el segundo argumento, usaríamos `print(sys.argv[1])`.

## Deep Dive en la lectura de argumentos de línea de comandos

Aparte de la lista `sys.argv`, también podemos usar el módulo `argparse` para leer argumentos de línea de comandos en Python. Este módulo nos permite crear argumentos con valores predeterminados, mensajes de ayuda y opciones adicionales. Aquí hay un ejemplo de cómo usar `argparse` para leer un argumento único:

```Python
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('--nombre', help='Especifica un nombre para saludar')

args = parser.parse_args()
print('¡Hola ' + args.nombre + '!')
```

Si ejecutamos este programa desde la línea de comandos de la siguiente manera:

```
python programa.py --nombre Carlos
```

Obtendremos la siguiente salida:

```
¡Hola Carlos!
```

## Ver también

- [Documentación oficial de Python para el módulo `sys`](https://docs.python.org/es/3/library/sys.html)
- [Documentación oficial de Python para el módulo `argparse`](https://docs.python.org/es/3/library/argparse.html)
- [Cómo usar el módulo `argparse` en Python](https://realpython.com/command-line-interfaces-python-argparse/)