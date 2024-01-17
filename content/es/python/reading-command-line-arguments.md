---
title:                "Leyendo argumentos de línea de comando"
html_title:           "Python: Leyendo argumentos de línea de comando"
simple_title:         "Leyendo argumentos de línea de comando"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

¿Qué es y por qué leer argumentos de línea de comando en Python?

La lectura de argumentos de línea de comando en Python se refiere a la capacidad del programa para recibir información o parámetros a través de la línea de comandos al momento de ser ejecutado. Los programadores suelen hacer uso de esta funcionalidad para dar más flexibilidad a sus programas y permitir al usuario personalizar su experiencia de uso.

## ¿Cómo hacerlo?

Para leer argumentos de línea de comando en Python, podemos utilizar el módulo `sys` el cual nos brinda una lista con todos los argumentos pasados al momento de ejecutar el programa. Aquí un ejemplo sencillo:

```Python
import sys

archivo = sys.argv[1] # el primer argumento se encuentra en la posición 1 de la lista
print("El archivo ingresado es:", archivo)
```

Si ejecutamos este programa con el comando `python programa.py archivo.txt`, el output sería:

```Python
El archivo ingresado es: archivo.txt
```

## Inmersión profunda

La lectura de argumentos de línea de comando es una funcionalidad básica que ha estado presente en lenguajes de programación desde hace mucho tiempo. En Python, esta funcionalidad está disponible gracias al módulo `sys`. Sin embargo, existen también otros módulos que facilitan esta tarea, como por ejemplo `argparse` que nos permite definir opciones y argumentos más complejos a través de código.

Además, es importante mencionar que la lectura de argumentos de línea de comando tiene una gran importancia en la automatización de tareas y la creación de scripts, ya que nos permite ejecutar comandos de forma más dinámica.

## Ver también

Si quieres saber más sobre cómo utilizar argumentos de línea de comando en Python, puedes consultar la documentación oficial del módulo `sys` y del módulo `argparse`. También puedes leer sobre cómo automatizar tareas con Python en nuestro artículo sobre programación de scripts.