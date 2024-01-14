---
title:    "C#: Obteniendo la fecha actual"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué
En el mundo de la programación, tener la habilidad para obtener la fecha y hora actual es esencial en muchas situaciones. Ya sea para registrar el tiempo de un evento, generar un archivo con la fecha en su nombre o simplemente para fines de organización, siempre es importante tener acceso a esta información en el código. En este artículo, aprenderemos cómo obtener la fecha actual en C# de una manera sencilla y efectiva.

## Cómo hacerlo
Para obtener la fecha actual en C#, podemos utilizar la clase `DateTime`. Esta clase contiene métodos útiles para trabajar con fechas y horas, incluyendo `Now()` que nos devuelve la fecha y hora actuales.
```C#
DateTime currentTime = DateTime.Now;
```
También podemos especificar un formato específico para la fecha utilizando el método `ToString()`:
```C#
DateTime currentTime = DateTime.Now;
string currentDate = currentTime.ToString("dd-MM-yyyy"); // en formato día-mes-año
```
Podemos hacer lo mismo para obtener solo la hora actual:
```C#
DateTime currentTime = DateTime.Now;
string currentTime = currentTime.ToString("HH:mm:ss"); // en formato de 24 horas
```
Podemos imprimir la fecha y hora actuales en la consola con un simple `Console.WriteLine()`:
```C#
Console.WriteLine("La fecha y hora actuales son: " + currentTime);
```
La salida sería algo como:
```
La fecha y hora actuales son: 29-06-2021 13:45:23
```

## Profundizando
Si queremos obtener información más detallada sobre la fecha actual, podemos utilizar otros métodos de la clase `DateTime`. Por ejemplo, `DateTime.Today` nos devuelve solo la fecha actual sin la hora, mientras que `DateTime.UtcNow` nos devuelve la hora universal coordinada (UTC).

También podemos combinar la fecha y hora actual con otras operaciones para realizar acciones específicas en nuestro código. Por ejemplo, podemos comparar la fecha actual con otra fecha específica para saber cuántos días han pasado. Podemos sumar o restar días, meses o años a la fecha actual utilizando los métodos `Add()` y `Subtract()`, o utilizar la clase `TimeSpan` para calcular la diferencia entre dos fechas.

## Ver también
- [Documentación de Microsoft sobre la clase DateTime](https://docs.microsoft.com/es-es/dotnet/standard/datetime/?view=netframework-4.7.2)
- [Tutoriales de C# en Español](https://www.tutorialspoint.com/csharp/index.htm)
- [GitHub repository para ejemplos de código de C#](https://github.com/TutorialRepository/CSharp)