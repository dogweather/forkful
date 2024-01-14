---
title:                "Python: Escribir pruebas"
programming_language: "Python"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-tests.md"
---

{{< edit_this_page >}}

## Porqué deberías escribir pruebas en Python

Escribir pruebas en Python es esencial para asegurar que nuestro código funcione correctamente y para detectar fallos de manera temprana en el proceso de desarrollo. Además, al escribir pruebas, podemos tener una mayor confianza en nuestro código y estar seguros de que se comportará de manera esperada en diferentes situaciones.

## Cómo escribir pruebas en Python

Para escribir pruebas en Python, utilizamos la librería "unittest", que nos permite crear pruebas automatizadas para diferentes partes de nuestro código. Veamos un ejemplo sencillo de cómo escribir una prueba para una función que suma dos números:

```
import unittest

def sumar(a, b):
    return a + b

class PruebasSuma(unittest.TestCase):

    def test_suma_positivos(self):
        resultado = sumar(2, 3)
        self.assertEqual(resultado, 5)

    def test_suma_negativos(self):
        resultado = sumar(-2, -4)
        self.assertEqual(resultado, -6)
```

En este ejemplo, creamos una clase para nuestras pruebas llamada "PruebasSuma" que hereda de la clase "unittest.TestCase". Dentro de esta clase, definimos dos métodos que comienzan con "test_" y realizan diferentes pruebas utilizando la función "sumar". En este caso, utilizamos el método "assertEquals" para asegurar que el resultado de la suma sea el esperado.

Una vez que hemos escrito nuestras pruebas, simplemente ejecutamos nuestro archivo y veremos si todas las pruebas pasan correctamente.

## Profundizando en la escritura de pruebas

El uso de pruebas en Python no solo nos permite detectar errores, sino que también nos ayuda a diseñar un código más limpio y organizado. Al escribir pruebas, tenemos que pensar en diferentes casos de uso para nuestra función o clase, lo que nos obliga a tener una mejor comprensión de nuestro código y a mejorar su calidad.

Además, en lugar de probar manualmente cada parte de nuestro código, las pruebas automatizadas nos ahorran tiempo y nos permiten verificar constantemente si nuestro código sigue funcionando de manera correcta.

## Otros recursos sobre escritura de pruebas en Python

- Documentación oficial de Python sobre pruebas: https://docs.python.org/es/3/library/unittest.html
- Tutorial de RealPython sobre escribir pruebas en Python: https://realpython.com/python-testing/
- Artículo sobre las ventajas de escribir pruebas en Python: https://www.geeksforgeeks.org/advantages-of-writing-mock-tests-in-python/

## Véase también
- [Documentación oficial de unittest en Python](https://docs.python.org/es/3/library/unittest.html)
- [Tutorial de RealPython sobre escribir pruebas en Python](https://realpython.com/python-testing/)
- [Artículo sobre las ventajas de escribir pruebas en Python](https://www.geeksforgeeks.org/advantages-of-writing-mock-tests-in-python/)