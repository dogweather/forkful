---
title:                "Escribiendo pruebas"
aliases:
- /es/c-sharp/writing-tests.md
date:                  2024-02-03T19:30:10.813394-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escribiendo pruebas"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/writing-tests.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Qué y Por Qué?

Escribir pruebas en C# implica crear scripts automatizados para validar la funcionalidad de tu código, asegurando que se comporte como se espera. Los programadores lo hacen para detectar errores tempranamente, facilitar la refactorización del código y asegurar que los nuevos cambios no interrumpan las funciones existentes, aumentando así la calidad y fiabilidad del software.

## Cómo hacerlo:

Los desarrolladores de C# usan principalmente los marcos de trabajo NUnit o xUnit para escribir pruebas debido a su flexibilidad y extenso conjunto de características. Aquí hay un ejemplo básico utilizando NUnit para probar una simple función de adición:

1. **Instala NUnit y NUnit3TestAdapter** a través del Administrador de Paquetes NuGet o la CLI de .NET:
```powershell
dotnet add package NUnit
dotnet add package NUnit3TestAdapter
```

2. **Crea un proyecto de biblioteca de clases C#** si aún no lo has hecho.

3. **Escribe una función simple** para probar. Por ejemplo, un método de adición en una clase llamada `Calculator`:
```csharp
public class Calculator
{
    public int Add(int a, int b)
    {
        return a + b;
    }
}
```

4. **Escribe una clase de prueba** usando NUnit:
```csharp
using NUnit.Framework;

namespace CalculatorTests
{
    [TestFixture]
    public class CalculatorTests
    {
        [Test]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange (Preparar)
            var calculator = new Calculator();
            int expected = 5;

            // Act (Actuar)
            int actual = calculator.Add(2, 3);

            // Assert (Afirmar)
            Assert.AreEqual(expected, actual);
        }
    }
}
```

5. **Ejecuta la prueba** usando el ejecutor de pruebas de tu IDE o la CLI de .NET:
```powershell
dotnet test
```

### Salida de Muestra:

Asumiendo que tu prueba sea exitosa, deberías ver una salida similar a esta:
```
Test Run Successful.
Total tests: 1
     Passed: 1
 Total time: 1.2345 Seconds
```

### Usando xUnit:

Si prefieres xUnit, la configuración es similar a NUnit. Así es como reescribirías el ejemplo de prueba para la clase `Calculator` usando xUnit:

1. **Instala xUnit y xUnit.runner.visualstudio**:
```powershell
dotnet add package xUnit
dotnet add package xUnit.runner.visualstudio
```

2. **Escribe una clase de prueba usando xUnit**:
```csharp
using Xunit;

namespace CalculatorTests
{
    public class CalculatorTests
    {
        [Fact]
        public void Add_AddsTwoIntegers_ReturnsCorrectSum()
        {
            // Arrange (Preparar)
            var calculator = new Calculator();
            int expected = 5;

            // Act (Actuar)
            int actual = calculator.Add(2, 3);

            // Assert (Afirmar)
            Assert.Equal(expected, actual);
        }
    }
}
```

3. **Ejecuta la prueba usando la CLI de .NET** o el ejecutor de pruebas integrado en tu IDE.

Tanto NUnit como xUnit ofrecen características poderosas para pruebas parametrizadas, operaciones de configuración/finalización, y organización de pruebas en categorías, haciéndolos herramientas indispensables en el conjunto de herramientas del programador C# para asegurar la calidad y funcionalidad del código.
