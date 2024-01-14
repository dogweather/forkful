---
title:    "C#: Escribiendo pruebas"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué escribir pruebas en C#

Escribir pruebas es una práctica común y esencial en el desarrollo de software en C#. Las pruebas nos permiten garantizar que nuestro código funciona correctamente y prevenir posibles errores antes de que lleguen a producción.

## Cómo escribir pruebas en C#

```C#
using System;
using Xunit;

namespace Pruebas_Unitarias
{
    public class CalculadoraTests
    {
        [Fact]
        public void Sumar_DosNumeros_DebeDevolverLaSuma()
        {
            //Arrange
            var calculadora = new Calculadora();
            var num1 = 5;
            var num2 = 10;
            var resultadoEsperado = 15;

            //Act
            var resultado = calculadora.Sumar(num1, num2);

            //Assert
            Assert.Equal(resultadoEsperado, resultado);
        }
    }
}
```

En este ejemplo, estamos probando el método "Sumar" de una clase llamada "Calculadora". Primero, creamos una instancia de la clase y definimos los valores de entrada y el resultado esperado. Luego, llamamos al método y comparamos el resultado obtenido con el esperado usando el método "Equal" de la clase Assert.

## Profundizando en las pruebas en C#

Las pruebas en C# se pueden escribir utilizando diferentes frameworks, como NUnit, xUnit o MSTest. Además, existen diferentes técnicas y patrones para escribir pruebas eficaces, como pruebas unitarias, de integración o de aceptación. También es importante entender los conceptos de mocks y stubs para simular ciertas dependencias en nuestras pruebas.

## Ver también

- [xUnit.net](https://xunit.net/)
- [Pruebas Unitarias en C#](https://geeks.ms/diaz/2015/06/11/introduccion-al-testing-para-desarrolladores-parte1/)
- [Técnicas de Pruebas en C#](https://marcomendez.net/pruebas-unitarias-en-net/)