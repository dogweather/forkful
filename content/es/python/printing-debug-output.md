---
title:    "Python: Imprimir salida de depuración"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por qué
Si eres un programador o estás aprendiendo a programar, probablemente hayas escuchado sobre la importancia de imprimir el resultado de tu código y cómo puede ayudarte a detectar errores. Pero, ¿por qué realmente se recomienda imprimir la salida de depuración? En esta publicación de blog, explicaremos por qué es útil imprimir la salida de depuración en tus programas de Python.

## Cómo hacerlo
Si estás utilizando Python 3, puedes imprimir la salida de depuración utilizando la función `print()`. Esta función acepta una o más expresiones como argumentos y las imprime en la consola. Por ejemplo, si quieres imprimir una cadena de texto, puedes hacerlo de la siguiente manera:

```Python
print("¡Hola, mundo!")
```

La salida de este código sería `¡Hola, mundo!`, que se mostrará en la consola. También puedes imprimir variables y expresiones matemáticas. Echa un vistazo a los siguientes ejemplos:

```Python
nombre = "Juan"
edad = 28

print("Mi nombre es", nombre)
print("Mi edad es", edad)
print("El doble de mi edad es", edad * 2)
```

La salida de este código sería:

```
Mi nombre es Juan
Mi edad es 28
El doble de mi edad es 56
```

Esto puede ser muy útil para asegurarte de que tus variables están almacenando los valores correctos en diferentes puntos de tu programa.

## Profundizando
Imprimir la salida de depuración también puede ayudarte a detectar errores y comprender mejor cómo funciona tu código. Al imprimir diferentes valores y expresiones, puedes ver cómo cambian a medida que tu programa se ejecuta. Esto te brinda una forma más visual de seguir el flujo de tu programa, lo que puede ser especialmente útil cuando trabajas con ciclos y estructuras de control.

Además, imprimir la salida de depuración te permite comprender mejor el comportamiento de tu código y te ayuda a identificar problemas potenciales. Puedes utilizarlo para verificar si tus cálculos matemáticos son correctos o si tus funciones están retornando los valores correctos.

## Ver también
¡Esperamos que hayas encontrado útil esta publicación sobre cómo imprimir la salida de depuración en Python! Si quieres seguir aprendiendo sobre debugueo y otras técnicas de programación, aquí hay algunos recursos adicionales para ti:

- [Documentación de Python sobre la función `print()`](https://docs.python.org/es/3/library/functions.html#print)
- [Python Debugging Techniques: Tips and Tricks](https://realpython.com/python-debugging-tips/), artículo de Real Python en inglés
- [Depuración en Python: Guía básica](https://www.linuxjournal.com/content/python-debugging-basics), guía básica de Linux Journal en inglés