---
date: 2024-01-26 00:56:32.380225-07:00
description: "C\xF3mo hacerlo: ."
lastmod: '2024-03-13T22:44:58.620037-06:00'
model: gpt-4-1106-preview
summary: .
title: Manejo de errores
weight: 16
---

## Cómo hacerlo:
``` Python
# Bloque básico try-except
try:
    # código riesgoso
    numero = int(input("Ingresa un número: "))
except ValueError:
    # manejar error
    print("¡Eso no es un número!")

# Especificando múltiples excepciones
try:
    # código que podría generar diferentes excepciones
    resultado = 10 / int(input("Ingresa un divisor: "))
except ZeroDivisionError:
    print("¡Ups! No se puede dividir entre cero.")
except ValueError:
    print("Necesito un número, amigo.")

# Usando else y finally
try:
    numero = int(input("Ingresa un número para elevar al cuadrado: "))
except ValueError:
    print("¡Dije un número!")
else:
    # no ocurrieron errores
    print("Tu número al cuadrado es:", numero**2)
finally:
    # siempre se ejecuta
    print("¡Gracias por probar esto!")
```

Ejemplo de salida al introducir un número inválido en el primer bloque:
```
Ingresa un número: hola
¡Eso no es un número!
```

## Inmersión Profunda
Desde los albores de la programación, el manejo de errores ha sido crucial. Los enfoques tempranos eran rudimentarios, como verificar condiciones antes de cada operación riesgosa. La sintaxis `try-except` de Python proviene de un legado de manejo de excepciones en lenguajes más antiguos como C++ y Java, simplificando el proceso.

Cuando intentas (`try`) un bloque de código, Python está atento a cualquier excepción. Si aparece un error, el bloque `except` lo captura. Puedes especificar sobre las excepciones que capturas o capturarlas todas con un `except` desnudo. Sin embargo, la especificación primero es el mejor enfoque: es preciso, no una red para todo.

`else` y `finally` son extras en este concepto. El bloque `else` se ejecuta si el bloque try no tiene errores. `finally` es el amigo confiable que se ejecuta pase lo que pase: piensa en operaciones de limpieza.

¿Alternativas? Claro que las hay. Algunos lenguajes utilizan códigos de retorno en lugar de excepciones. También podrías encontrar sentencias `with` para el manejo de recursos o `assertions` que verifican condiciones mientras se desarrolla. Pero cuando hablamos de estrategias de manejo de errores sólidas, el modelo try-catch destaca por su legibilidad y estructura.

## Ver También
Aquí hay algunos buenos recursos adicionales para profundizar aún más:

- Documentación oficial de Python sobre errores y excepciones: [Python Docs – Errors and Exceptions](https://docs.python.org/3/tutorial/errors.html)
- Guía de Real Python sobre el tema: [Real Python - The try/except/else/finally block](https://realpython.com/python-exceptions/)
- Una discusión reflexiva sobre las mejores prácticas de manejo de errores: [Stack Overflow – ¿Cómo ignoro correctamente las excepciones?](https://stackoverflow.com/questions/4990718/about-catching-any-exception)
