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

## ¿Qué y por qué?

Escribir pruebas de código es un proceso clave en el desarrollo de software que implica crear y ejecutar casos de prueba para garantizar que el código funcione correctamente. Los programadores realizan este proceso para detectar y corregir errores en su código antes de que llegue a los usuarios finales, lo que a su vez mejora la calidad y estabilidad del software.

## Cómo:

El primer paso para escribir pruebas en C# es asegurarse de tener un proyecto de Visual Studio creado y una clase de prueba en la que se puedan escribir las pruebas. A continuación, se deben importar los espacios de nombres necesarios para escribir las pruebas:

```C#
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
```

A continuación, se puede proceder a escribir las pruebas utilizando los diferentes atributos y métodos proporcionados por el marco de pruebas de Visual Studio. Por ejemplo, para probar una función que devuelve un número entero, se puede utilizar el atributo ```[TestMethod]``` y el método de aserción ```Assert.AreEqual()```:

```C#
[TestMethod]
public void TestSum() 
{
    Assert.AreEqual(10, sum(5, 5)); //se espera que la suma de 5 y 5 sea igual a 10
}
```

Al ejecutar las pruebas, se obtendrá una salida similar a la siguiente:

```
Passed! 
Total tests: 1 
Passed: 1 
Failed: 0 
Time taken: 0.04 seconds
```

## Profundizando:

La práctica de escribir pruebas de código va de la mano con el desarrollo de software y es esencial para garantizar la seguridad y calidad de los programas. Existen diferentes marcos y herramientas para escribir pruebas, como NUnit o xUnit, que ofrecen características adicionales y flexibilidad. Además, escribir pruebas ayuda a la detección temprana de errores en el código, lo que a su vez reduce el tiempo y el costo necesarios para arreglarlos más adelante en el proceso de desarrollo.

## Ver también:

- [Mastering Visual Studio Testing Tools](https://docs.microsoft.com/en-us/visualstudio/test/walkthrough-creating-and-running-unit-tests-for-managed-code?view=vs-2019)
- [Unit Testing C# Code](https://www.geeksforgeeks.org/unit-testing-of-c-code/)

___
**Nota del traductor:** Este artículo se ha escrito en un estilo informal y directo, evitando palabras y frases innecesarias para una lectura fluida y concisa.