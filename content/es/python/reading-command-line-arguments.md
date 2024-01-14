---
title:                "Python: Leyendo argumentos de línea de comando"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## ¿Por qué leer argumentos de línea de comando?

Si eres un programador de Python, es probable que ya hayas escuchado hablar sobre argumentos de línea de comando. Aunque pueden parecer un concepto técnico y complejo, en realidad son una herramienta muy útil para automatizar y hacer más eficiente el proceso de ejecutar un programa. En esta entrada de blog, te explicaremos por qué deberías aprender a trabajar con estos argumentos y cómo hacerlo en Python.

## Cómo hacerlo

Primero, veamos un ejemplo sencillo de cómo utilizar argumentos de línea de comando en un programa de Python:

```
import sys

# el primer argumento después del nombre del archivo es el número de veces que se imprimirá el mensaje
for i in range(int(sys.argv[1])):
  print("¡Hola, mundo!")
```
El programa anterior toma un argumento de línea de comando, lo convierte en un número entero y lo utiliza para imprimir un mensaje determinado número de veces. Ahora, si ejecutamos este programa en la línea de comando con un número como argumento, obtendremos el siguiente resultado:

```
$ python programa.py 3
¡Hola, mundo!
¡Hola, mundo!
¡Hola, mundo!
```

Como puedes ver, al proporcionar un argumento de línea de comando, podemos personalizar la salida de nuestro programa de acuerdo a nuestras necesidades. Esto es especialmente útil cuando tenemos un script que queremos ejecutar varias veces con distintos valores sin tener que cambiar manualmente el código cada vez.

## Inmersión Profunda

Entonces, ¿cómo funcionan realmente los argumentos de línea de comando en Python? En resumen, cuando ejecutamos un programa de Python desde la línea de comando, cualquier texto que escribamos después del nombre del archivo será almacenado en una lista llamada `sys.argv.` Esta lista contiene todos los argumentos de la línea de comando, y el primer elemento siempre será el nombre del archivo en sí.

Sin embargo, si queremos trabajar con los argumentos de manera más amigable, podemos utilizar el módulo `argparse` de Python. Este módulo nos permite definir cuáles son los argumentos que aceptaremos, asignarles valores predeterminados y obtener su valor de manera más fácil y legible en nuestro código.

## Ver también

- [Documentación oficial de Python sobre argumentos de línea de comando](https://docs.python.org/es/3/howto/argparse.html)
- [Tutorial de programación Python: Argumentos de línea de comando](https://www.tutorialspoint.com/python/python_command_line_arguments.htm)
- [Artículo sobre la importancia de utilizar argumentos de línea de comando en Python](https://realpython.com/python-command-line-arguments/)