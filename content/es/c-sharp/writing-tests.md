---
title:                "C#: Escribiendo pruebas"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué

Escribir pruebas es una práctica clave en el mundo de la programación, ya que permite asegurarse de que nuestro código funcione correctamente y sin errores. Además, es una forma eficiente de detectar y solucionar problemas en una etapa temprana del proceso de desarrollo.

## Cómo hacerlo

Para escribir pruebas en C#, es necesario utilizar un marco de pruebas como NUnit o xUnit. A continuación, se presenta un ejemplo de cómo escribir una simple prueba de suma en NUnit:

```C#
[TestFixture]
public class CalculadoraTests
{
    [Test]
    public void Suma_DosNumeros_ValorEsperado()
    {
        // Arrange
        Calculadora calculadora = new Calculadora();

        // Act
        int resultado = calculadora.Sumar(2, 3);

        // Assert
        Assert.AreEqual(5, resultado);
    }
}
```

En este caso, se utiliza la clase `Calculadora` que contiene el método `Sumar` que realiza la operación deseada. En la prueba, se crea una instancia de la clase y se invoca el método con los parámetros adecuados. Luego, se utiliza `Assert` para verificar si el resultado es el esperado. En este ejemplo, si el resultado obtenido es 5, la prueba pasa exitosamente.

## Profundizando

Al escribir pruebas, es importante tomar en cuenta ciertos aspectos, como por ejemplo la cobertura de código. Esto se refiere a la cantidad de código que está siendo ejecutado por las pruebas. Se recomienda que la cobertura sea lo más cercana posible al 100%, ya que esto garantiza que todas las líneas de código están siendo probadas. Además, es importante tener en cuenta los distintos tipos de pruebas, como las de unidad, integración y funcionales, y utilizarlas de manera adecuada en el proceso de desarrollo.

## Ver también

- [NUnit](https://nunit.org/)
- [xUnit](https://xunit.net/)
- [Cobertura de código en C#](https://docs.microsoft.com/es-es/dotnet/visual-studio/test/understand-code-coverage-results?view=vs-2019)
- [Tipos de pruebas en el desarrollo de software](https://www.codeproject.com/Articles/1256524/A-Deep-Dive-Into-Types-of-Testing-In-Software-Deve)