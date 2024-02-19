---
aliases:
- /es/python/using-an-interactive-shell-repl/
date: 2024-01-26 04:16:49.311817-07:00
description: "Un REPL, o Bucle de Leer-Evaluar-Imprimir, es un entorno de programaci\xF3\
  n que toma entradas individuales del usuario, las ejecuta y devuelve el resultado\u2026"
lastmod: 2024-02-18 23:09:09.557974
model: gpt-4-0125-preview
summary: "Un REPL, o Bucle de Leer-Evaluar-Imprimir, es un entorno de programaci\xF3\
  n que toma entradas individuales del usuario, las ejecuta y devuelve el resultado\u2026"
title: Usando una shell interactiva (REPL)
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Un REPL, o Bucle de Leer-Evaluar-Imprimir, es un entorno de programación que toma entradas individuales del usuario, las ejecuta y devuelve el resultado al usuario. Los programadores lo utilizan para pruebas rápidas, aprender, depurar o hacer cálculos sobre la marcha.

## Cómo:
Salta directamente al REPL de Python escribiendo `python` en tu línea de comandos. Una vez allí, prueba operaciones simples o código de varias líneas:

```Python
>>> 1 + 1
2
>>> for i in range(3):
...     print(i)
...
0
1
2
```

Experimenta con funciones y retroalimentación inmediata:

```Python
>>> def greet(name):
...     return "Hola, " + name + "!"
...
>>> greet("Alice")
'Hola, Alice!'
```

Juega con bibliotecas y explora sus características en tiempo real:

```Python
>>> import math
>>> math.sqrt(16)
4.0
```

Sal con un rápido `exit()` o `Ctrl+D` (a veces `Ctrl+Z` en Windows).

## Profundización
El concepto de un REPL no es único de Python; es tan antiguo como Lisp. Muchos lenguajes ofrecen este entorno inmediato e interactivo para un enfoque práctico del código. Alternativas a la shell nativa de Python incluyen IPython y Jupyter Notebook, que proporcionan mayor interactividad, más características y mejor integración con otras herramientas. El REPL estándar de Python es simple, pero incorpora todo el poder de Python, manejando objetos complejos y programas multihilo, aunque carece de características como autocompletado y resaltado de sintaxis presentes en herramientas más avanzadas.

## Ver También
- [Documentación oficial de Python sobre el intérprete](https://docs.python.org/3/tutorial/interpreter.html)
- [IPython: Una shell avanzada de Python](https://ipython.org/)
- [Proyecto Jupyter](https://jupyter.org/)
