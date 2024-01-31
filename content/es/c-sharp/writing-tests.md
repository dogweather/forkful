---
title:                "Escribiendo pruebas"
date:                  2024-01-19
html_title:           "Arduino: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"

category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir tests significa crear código que verifica que otros códigos funcionan como deben. Programadores los usan para asegurarse de que sus aplicaciones son estables y para prevenir fallas futuras al modificar el código.

## Cómo hacerlo:

Para escribir pruebas en C#, se utiliza principalmente el framework NUnit. A continuación, un ejemplo de cómo implementar una prueba unitaria básica con NUnit:

```C#
using NUnit.Framework;

namespace Tests
{
    public class CalculosTest
    {
        [Test]
        public void Suma_DeberiaRetornarSumaCorrecta()
        {
            // Arrange
            var a = 5;
            var b = 10;
            var esperado = 15;

            // Act
            var resultado = Suma(a, b);

            // Assert
            Assert.AreEqual(esperado, resultado);
        }

        private int Suma(int num1, int num2)
        {
            return num1 + num2;
        }
    }
}
```

Y así se ve la salida cuando la prueba pasa exitosamente:

```
Test Passed: Suma_DeberiaRetornarSumaCorrecta
```

## Deep Dive

Historia: Kent Beck creó el desarrollo impulsado por pruebas (TDD) en la década de 1990. TDD promueve escribir tests antes del código de producción.

Alternativas: A parte de NUnit, existen otros frameworks como MSTest y xUnit en el mundo de .NET para pruebas unitarias.

Detalles de Implementación: Al escribir tests, es importante mantenerlos aislados y enfocados. Utiliza mocks y stubs para simular comportamientos externos.

## Ver también:

- Documentación oficial de NUnit: https://nunit.org/
- Tutorial de TDD en C#: https://docs.microsoft.com/en-us/learn/modules/test-driven-development-csharp/
- Comparación entre frameworks de tests para .NET: https://dzone.com/articles/comparing-net-unit-testing-frameworks
