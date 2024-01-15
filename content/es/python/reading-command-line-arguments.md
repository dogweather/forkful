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

## Por qué

¿Te has preguntado alguna vez cómo los programas y scripts en Python pueden recibir información directamente desde la línea de comandos? ¡Eso es gracias a los argumentos de línea de comandos! En este artículo, aprenderás por qué es importante saber cómo leer y usar estos argumentos en tus proyectos de Python.

## Cómo

Es súper fácil leer los argumentos de línea de comandos en Python. Todo lo que necesitas es utilizar el módulo `sys` y la función `argv`. ¡Echemos un vistazo a un ejemplo!

```Python
import sys

# Imprime los argumentos de línea de comandos
print(sys.argv)
```

Si ejecutamos este código usando `python script.py argumento1 argumento2`, obtendremos la siguiente salida:

```Python
['script.py', 'argumento1', 'argumento2']
```

Genial, ¿verdad? Así es como puedes acceder a los argumentos de línea de comandos en tu script. Pero, ¿qué pasa si solo quieres uno de los argumentos? ¡Sigue leyendo para descubrirlo!

## Deep Dive

Además de la función `argv`, el módulo `sys` también nos proporciona la función `arg`, que nos permite acceder a los argumentos individuales de manera más fácil. También podemos utilizar el operador de indexación `[]` para acceder a un argumento específico por su posición en la lista.

Veamos un ejemplo de cómo podemos utilizar esto:

```Python
import sys

# Acceder al tercer argumento de línea de comandos
print(sys.argv[2])

# Utilizar la función arg para acceder al segundo argumento
print(sys.arg[1])
```

En este caso, si ejecutamos el código usando `python script.py argumento1 argumento2 argumento3`, obtendremos la siguiente salida:

```
argumento2
argumento2
```

Ahora ya sabes cómo leer y acceder a los argumentos de línea de comandos en tus proyectos de Python. Puedes utilizar esta información para crear scripts más dinámicos y versátiles que puedan recibir diferentes entradas cada vez que se ejecutan.

## Ver También

- [Documentación oficial de Python sobre módulo `sys`](https://docs.python.org/es/3/library/sys.html)
- [Tutorial de Programiz sobre argumentos de línea de comandos en Python](https://www.programiz.com/python-programming/command-line-arguments)