---
title:    "C#: Calculando una fecha en el futuro o pasado"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular fechas en el pasado o futuro?

La programación en C# puede parecer un poco intimidante al principio, pero una vez que entiendes los conceptos básicos, hay una gran cantidad de aplicaciones útiles que puedes crear. Una de estas aplicaciones es la capacidad de calcular fechas en el pasado o en el futuro. Esto puede ser útil en una variedad de situaciones, como planificar eventos, gestionar plazos o simplemente jugar con diferentes escenarios temporales.

## Cómo hacerlo

Para calcular una fecha en el pasado o futuro, necesitarás dos cosas: una fecha base y un número de días para agregar o restar. Por ejemplo, si queremos calcular la fecha que será dentro de 7 días a partir de hoy, tendríamos que definir la fecha base como la fecha actual y el número de días en 7. Podemos representar esto en código de la siguiente manera:

```C#
DateTime fechaBase = DateTime.Today;
int dias = 7;

DateTime fechaCalculada = fechaBase.AddDays(dias);
```

Esto devolverá una nueva fecha que será exactamente 7 días después de la fecha base. También podemos hacer cálculos en el pasado simplemente cambiando el signo del número de días, como en el siguiente ejemplo para obtener la fecha que fue 5 días antes de hoy:

```C#
DateTime fechaBase = DateTime.Today;
int dias = -5;

DateTime fechaCalculada = fechaBase.AddDays(dias);
```

## Profundizando en los detalles

Sin embargo, si estás buscando un poco más de flexibilidad en tus cálculos de fechas, hay otras propiedades y métodos que puedes utilizar para obtener resultados más precisos. Por ejemplo, en lugar de sumar o restar días, también puedes hacerlo con semanas, meses o incluso años. O si necesitas tener en cuenta los días festivos o fines de semana, puedes utilizar el método `AddBusinessDays()` en lugar de `AddDays()`. También puedes utilizar el método `AddHours(), AddMinutes(), AddSeconds()` para hacer cálculos con horas, minutos o segundos.

Otro detalle a tener en cuenta es que el objeto `DateTime` también tiene propiedades como `DayOfWeek` para obtener el día de la semana, `DayOfYear` para obtener el día del año y `IsLeapYear` para verificar si el año es bisiesto.

Usar estos métodos y propiedades puede ser útil si necesitas hacer cálculos más complejos con fechas, así que no tengas miedo de experimentar y probar diferentes combinaciones para lograr el resultado deseado.

## Ver también

- Documentación de Microsoft sobre `DateTime`: https://docs.microsoft.com/es-es/dotnet/api/system.datetime?view=net-5.0
- Tutorial en español sobre el manejo de fechas en C#: https://www.tutorialsteacher.com/csharp/csharp-datetime