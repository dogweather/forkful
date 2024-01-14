---
title:    "C#: Escribir pruebas"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## Por qué escribir pruebas en C#

En el mundo de la programación, escribir pruebas de software es una práctica común y recomendada. Si bien puede tomar un poco más de tiempo al principio, a largo plazo, te ahorrará tiempo y problemas al encontrar y solucionar errores en tu código. Además, proporciona una capa adicional de seguridad al garantizar que tu código funcione correctamente en todo momento.

## Cómo escribir pruebas en C#

Escribir pruebas en C# es sencillo una vez que entiendes cómo funciona. Necesitarás hacer uso de un marco de pruebas, como NUnit o xUnit, para escribir y ejecutar tus pruebas en un entorno controlado. Aquí hay un ejemplo sencillo de una prueba en C# utilizando NUnit:

```
using NUnit.Framework;

namespace EjemploPruebas
{
   [TestFixture]
   public class Prueba
   {
       [Test]
       public void SumaDosNumeros()
       {
           //Arrange
           int num1 = 5;
           int num2 = 10;
           int resultadoEsperado = 15;

           //Act
           int resultado = num1 + num2;

           //Assert
           Assert.AreEqual(resultadoEsperado, resultado);
       }
   }
}
```

En este ejemplo, hemos creado una prueba que verifica si la suma de dos números da como resultado el valor esperado. El primer paso es usar la etiqueta `[TestFixture]` para indicar que esta clase contiene pruebas. Luego, utilizamos la etiqueta `[Test]` antes de cada método de prueba y escribimos nuestro código de prueba entre las etiquetas de `Arrange`, `Act` y `Assert`.

## Profundizando en las pruebas en C#

Escribir pruebas en C# no se trata solo de verificar si tu código funciona como se espera. También te ayuda a mejorar tu código y a desarrollar habilidades de programación de alta calidad. Al escribir pruebas, estás obligado a pensar en cómo funciona tu código y cómo podrías mejorarlo. Además, te ayuda a encontrar y solucionar errores de manera más eficiente, lo que ayuda a crear un código más robusto y confiable.

Además, al automatizar tus pruebas con un marco de pruebas, puedes ejecutarlas en cualquier momento con solo un clic, lo que ahorra mucho tiempo y evita errores humanos al realizar pruebas manuales repetitivas.

## Ver también

- [Documentación de NUnit](https://docs.nunit.org/)
- [Documentación de xUnit](https://xunit.net/docs/getting-started/netfx/visual-studio)
- [Beneficios de escribir pruebas en C#](https://exceptionnotfound.net/introduction-to-unit-testing-in-csharp-with-nunit/)