---
title:                "Refactorización"
aliases: - /es/c-sharp/refactoring.md
date:                  2024-01-26T01:17:12.061535-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactorización"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/refactoring.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

La refactorización es el proceso de reestructuración del código de computadora existente sin cambiar su comportamiento externo. Los programadores lo hacen para limpiar el código, mejorar la legibilidad, reducir la complejidad y mejorar la mantenibilidad.

## Cómo hacerlo:

Vamos a refactorizar un método simple de C# que calcula e imprime la suma de un arreglo de números:

Antes de Refactorizar:
```C#
public class Calculator
{
    public void CalculateSum()
    {
        int[] numbers = { 1, 2, 3, 4, 5 };
        int sum = 0;
        for (int i = 0; i < numbers.Length; i++)
        {
            sum += numbers[i];
        }
        Console.WriteLine("La suma es " + sum);
    }
}
```

Después de Refactorizar:
```C#
public class Calculator
{
    private readonly int[] _numbers;

    public Calculator(int[] numbers)
    {
        _numbers = numbers;
    }

    public int CalculateSum()
    {
        return _numbers.Sum();
    }

    public void DisplaySum()
    {
        Console.WriteLine($"La suma es {CalculateSum()}");
    }
}

// Uso:
var calculator = new Calculator(new[] { 1, 2, 3, 4, 5 });
calculator.DisplaySum();
```

Mediante la refactorización, hemos separado las preocupaciones, hecho que la clase `Calculator` sea más flexible al permitirle recibir cualquier arreglo de números, y aprovechado LINQ para hacer el cálculo de la suma más conciso.

## Análisis Profundo

La refactorización tiene sus raíces en la comunidad de programación de smalltalk y fue popularizada en los años 90 por el libro de Martin Fowler "Refactoring: Improving the Design of Existing Code". A lo largo de los años, se ha convertido en una parte fundamental de las metodologías ágiles y las buenas prácticas de codificación.

Existen varios enfoques para la refactorización, como el Red-Green-Refactor en el Desarrollo Dirigido por Pruebas (TDD). Esto asegura que la refactorización no introduzca errores al comenzar con una prueba fallida, hacer que pase y luego limpiar el código.

Cuando se implementa la refactorización, es crucial contar con una suite de pruebas comprensiva para asegurar que no se rompa ninguna funcionalidad durante el proceso. Las herramientas de refactorización automatizadas, como ReSharper para C#, también pueden ayudar en este proceso al proporcionar formas seguras de cambiar las estructuras de código. Sin embargo, las herramientas deben ser suplementarias a una profunda comprensión de la base del código y los principios de codificación.

## Ver También

- El trabajo seminal de Martin Fowler sobre Refactorización: [Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
- Guía de Microsoft sobre Refactorización en Visual Studio: [Refactorización (C#)](https://docs.microsoft.com/en-us/visualstudio/ide/refactoring-in-visual-studio?view=vs-2022)
- Una mirada detallada a los patrones de Refactorización con ejemplos: [SourceMaking Refactoring](https://sourcemaking.com/refactoring)
