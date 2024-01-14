---
title:                "Python: Imprimiendo salida de depuración"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/printing-debug-output.md"
---

{{< edit_this_page >}}

# ¿Por qué imprimir la salida de depuración en Python?

Cuando se está programando en Python, es común encontrarse con errores o problemas en el código. En estos casos, imprimir la salida de depuración puede ser una herramienta útil para identificar y solucionar estos errores de manera eficiente.

## Cómo hacerlo:
La forma más fácil de imprimir la salida de depuración en Python es utilizando la función `print()`. A continuación, te presentamos un ejemplo de cómo se vería esta función en acción:
```
# Creación de una lista de números
numeros = [1, 2, 3, 4, 5]

# Utilizando la función print para imprimir cada número en la lista
for num in numeros:
    print(num)
```
La salida de este código sería la siguiente:
```
1
2
3
4
5
```
Como puedes ver, la función `print()` nos permite ver el valor de cada variable en un momento específico del código, lo que puede ser útil para detectar posibles errores.

## Una mirada más profunda:
Python también tiene una función llamada `pprint` que se utiliza para imprimir estructuras de datos de manera más organizada y fácil de leer. A continuación, se muestra un ejemplo de cómo se vería la salida de la función `pprint` para la misma lista de números utilizada anteriormente:
```
# Importando la librería pprint
import pprint

# Creación de una lista de números
numeros = [1, 2, 3, 4, 5]

# Utilizando la función pprint para imprimir la lista
pprint.pprint(numeros)
```
La salida de este código sería la siguiente:
```
[1, 2, 3, 4, 5]
```
Como puedes ver, la función `pprint` nos da una salida mucho más organizada y legible, lo que puede ser útil cuando se trata de estructuras de datos más complejas.

# Ver también:
- [Documentación oficial de Python sobre la función `print()`](https://docs.python.org/es/3/library/functions.html#print)
- [Documentación oficial de Python sobre la función `pprint`](https://docs.python.org/es/3/library/pprint.html)
- [Artículo de Real Python sobre la impresión de la salida de depuración en Python](https://realpython.com/python-debugging-pdb/)