---
title:                "Escribiendo pruebas"
html_title:           "C#: Escribiendo pruebas"
simple_title:         "Escribiendo pruebas"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-tests.md"
---

{{< edit_this_page >}}

## ¿Por qué escribir pruebas en C#?

Escribir pruebas en nuestro código puede parecer una tarea tediosa e innecesaria. Sin embargo, es una práctica importante que nos ayuda a asegurarnos de que nuestro código funcione correctamente y a prevenir errores en el futuro. Al escribir pruebas, estamos probando nuestro código desde diferentes perspectivas y asegurándonos de que todas las funcionalidades están correctamente implementadas.

## ¿Cómo hacerlo?

El proceso de escribir pruebas en C# es bastante sencillo. A continuación, presentamos un ejemplo de cómo escribir y ejecutar pruebas en un método simple que suma dos números:

```C#
using System;

public class Calculator
{
    public int Sum(int num1, int num2)
    {
        return num1 + num2;
    }
}

// Creación de una prueba para el método Sum
[Test]
public void Sum_Test()
{
    // Arrange
    Calculator calculator = new Calculator();

    // Act
    int result = calculator.Sum(2, 3);

    // Assert
    Assert.AreEqual(5, result);
}
```

En este ejemplo, estamos utilizando el framework de pruebas NUnit para escribir nuestra prueba. Primero, creamos una clase llamada "Calculator" que contiene un método llamado "Sum", el cual toma dos números como parámetros y devuelve su suma. Luego, creamos una prueba llamada "Sum_Test", donde utilizamos el método "Assert.AreEqual" para asegurarnos de que el resultado de la suma sea igual a 5 cuando pasamos los números 2 y 3 como argumentos al método "Sum".

Una vez que hemos escrito nuestra prueba, podemos ejecutarla y veremos si pasa o falla. Si el resultado de la suma es igual a 5, nuestra prueba pasará satisfactoriamente. En caso contrario, tendremos que revisar nuestro código y corregir posibles errores.

Es importante mencionar que se recomienda escribir pruebas tanto para casos de éxito como para casos de error. Esto nos ayuda a detectar y solucionar errores y a garantizar que nuestro código maneja todas las situaciones posibles.

## Profundizando en las pruebas en C#

Existen diferentes frameworks de pruebas para C#, como NUnit, xUnit o MSTest, que nos facilitan el proceso de escribir y ejecutar pruebas en nuestro código. Además, también es posible realizar pruebas unitarias, que nos permiten probar métodos individuales en lugar de todo el sistema.

Otra técnica útil es la integración de pruebas en el proceso de integración continua (CI), donde se ejecutan automáticamente todas las pruebas escritas cada vez que se realiza un cambio en el código. Esto nos ayuda a detectar errores de forma rápida y a garantizar que nuestro código sigue funcionando correctamente.

En resumen, escribir pruebas en C# es una práctica esencial para garantizar la calidad de nuestro código y prevenir futuros errores. Al seguir unos sencillos pasos y utilizar las herramientas adecuadas, podemos asegurarnos de que nuestro código es confiable y funciona como esperamos.

## Ver también

- [Guía de pruebas en C#](https://docs.microsoft.com/en-us/visualstudio/test/how-to-write-unit-tests-for-csharp?view=vs-2019)
- [xUnit framework de pruebas](https://xunit.net/)
- [Integración continua con Azure DevOps](https://azure.microsoft.com/es-es/services/devops/continuous-integration/)