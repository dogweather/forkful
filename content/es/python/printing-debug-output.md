---
title:                "Python: Impresión de salida de depuración"
simple_title:         "Impresión de salida de depuración"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué

El uso de la impresión de salida de depuración es una herramienta importante para los programadores de Python. Les permite revisar y analizar un código en ejecución, identificar errores y solucionar problemas. En pocas palabras, la impresión de salida de depuración es una forma efectiva de mejorar la calidad del código.

## Cómo hacerlo

El proceso de impresión de salida de depuración en Python es bastante simple. Primero, importa el módulo "pprint" y luego usa la función "pprint" para imprimir el objeto que quieres depurar. A continuación, utiliza la función "print()" para imprimir cualquier mensaje o variable que quieras mostrar.

```
import pprint

x = [1, 2, 3, 4, 5]
pprint.pprint(x)

print("Esto es un mensaje de depuración")
```

El ejemplo anterior imprimirá la lista "x" con un formato más legible gracias a la función "pprint", y también imprimirá el mensaje de depuración.

## Profundizando

La impresión de salida de depuración es una forma útil de obtener información sobre el comportamiento de un código mientras se está ejecutando. Además de la función básica de "pprint", hay varias opciones que pueden ayudar a mejorar la salida de depuración, como "depth" para controlar la cantidad de anidación permitida y "compact" para mostrar la salida en una sola línea.

Otra técnica útil es utilizar la función "repr()" en lugar de "print()" para mostrar una representación precisa del objeto. Esto es especialmente útil para imprimir variables de tipo de datos complejos como cadenas con comillas o listas vacías.

## Ver también

- [Documentación de Python sobre impresión de salida de depuración](https://docs.python.org/es/3/library/pprint.html)
- [Guía sobre depuración de código en Python](https://realpython.com/python-debugging-pdb/)
- [Tutorial de impresión de salida de depuración en YouTube](https://www.youtube.com/watch?v=mxUR8phKCrU)