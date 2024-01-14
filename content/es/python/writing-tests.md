---
title:    "Python: Escribiendo pruebas"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/python/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

Escribir pruebas es una parte importante de la programación en Python. No solo ayuda a asegurar que nuestro código funcione correctamente, sino que también puede ahorrar tiempo en el largo plazo al detectar errores antes de que se conviertan en problemas mayores.

## Cómo hacerlo

Para escribir pruebas en Python, podemos utilizar el módulo incorporado de `unittest`. Este módulo proporciona herramientas para definir y ejecutar pruebas en nuestro código. Veamos un ejemplo simple para probar una función que suma dos números enteros:

```Python
import unittest

def sumar(num1, num2):
    return num1 + num2

class PruebasSuma(unittest.TestCase):

    def test_suma_positivos(self):
        resultado = sumar(2, 3)
        self.assertEqual(resultado, 5) # verifica que el resultado sea igual a 5

    def test_suma_negativos(self):
        resultado = sumar(-2, -3)
        self.assertEqual(resultado, -5)

if __name__ == '__main__':
    unittest.main()
```

Aquí estamos importando el módulo `unittest` y definiendo una función `sumar` que simplemente suma dos números. Luego, creamos una clase `PruebasSuma` que hereda de `unittest.TestCase`, nuestra clase de pruebas.

Dentro de esta clase, definimos dos métodos de prueba, uno para sumar números positivos y otro para sumar números negativos. Utilizamos el método `assertEqual` para verificar si el resultado de la función `sumar` es igual al valor esperado. Finalmente, llamamos al método `unittest.main()` para ejecutar nuestras pruebas.

Si ejecutamos este código, veremos que ambas pruebas pasan, ya que el resultado de la suma es igual al valor esperado. Este es solo un ejemplo simple, pero con `unittest` podemos escribir pruebas más complejas para probar diferentes escenarios y casos de uso.

## Inmersión profunda

Además de `unittest`, también existen otras herramientas y librerías para escribir pruebas en Python, como `pytest` o `doctest`. Cada una tiene sus propias características y ventajas, por lo que es importante familiarizarse con ellas y elegir la que mejor se adapte a nuestras necesidades.

Al escribir pruebas, es importante tener en cuenta que no solo debemos probar el código que escribimos, sino también el código de otros. Podemos utilizar pruebas de integración para verificar que nuestras funciones y clases funcionen correctamente juntas.

También es recomendable seguir buenas prácticas al escribir pruebas, como mantenerlas simples y en pequeñas piezas de código, y mantenerlas actualizadas a medida que nuestro código cambia.

## Ver también

Enlaces útiles para aprender más sobre cómo escribir pruebas en Python:

- Documentación oficial de `unittest`: https://docs.python.org/3/library/unittest.html
- Tutorial de pruebas en Python: https://realpython.com/python-testing/
- Artículo sobre las ventajas de escribir pruebas: https://www.codementor.io/blog/python-test-driven-development-benefits-du107obi9