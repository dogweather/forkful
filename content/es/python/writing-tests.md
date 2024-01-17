---
title:                "Escribiendo pruebas"
html_title:           "Python: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Escribir pruebas (o "tests") en Python es una práctica común entre los programadores. Básicamente, se trata de crear un código que compruebe que nuestro programa funciona correctamente y no contiene errores. Es una manera de asegurarnos de que nuestro código es robusto y confiable.

## Cómo hacerlo:

Para escribir una prueba en Python, es necesario utilizar el módulo "unittest". Este módulo nos proporciona herramientas para crear pruebas y ejecutarlas. Podemos escribir pruebas para cada una de las funciones de nuestro programa y comprobar que los resultados son los esperados. Aquí hay un ejemplo de una prueba sencilla:

```Python
import unittest

def suma(a, b):
    return a + b

class PruebaSuma(unittest.TestCase):
    def prueba(self):
        res = suma(2, 2)
        self.assertEqual(res, 4)

if __name__ == '__main__':
    unittest.main()
```

El resultado de esta prueba será exitoso, ya que el resultado de la función "suma" es 4. En caso de que el resultado fuera diferente, la prueba fallaría y nos indicaría que algo está mal en nuestro código.

## Profundizando:

La escritura de pruebas no es algo nuevo en el mundo de la programación. Ya en los años 70, se utilizaban técnicas de prueba para asegurarse de que el código fuera correcto. Sin embargo, en aquel entonces se hacía de manera manual y no existían herramientas como el módulo "unittest" de Python. Hoy en día, existen otras alternativas como "pytest" o "nose" que también son muy utilizadas por los programadores.

## Ver también:

- [Documentación oficial de unittest en Python](https://docs.python.org/es/3/library/unittest.html)
- [Tutorial de unittest en Real Python](https://realpython.com/python-testing/#why-write-tests)