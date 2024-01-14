---
title:    "C#: Calculando una fecha en el futuro o pasado"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Por qué
¿Alguna vez te has preguntado cómo calcular la fecha en el futuro o en el pasado en tu programa de C#? Puede que necesites hacer cálculos de tiempo para determinar fechas de vencimiento, citas o eventos importantes. Afortunadamente, con algunas líneas de código en C#, puedes calcular fácilmente fechas futuras o pasadas según tus necesidades.

## Cómo
Para calcular una fecha en el futuro o en el pasado en C#, primero necesitas crear un objeto de tipo `DateTime` con la fecha actual como punto de partida. Luego, puedes utilizar el método `AddDays`, `AddMonths` o `AddYears` para sumar o restar la cantidad de días, meses o años deseados a la fecha actual. Por ejemplo:

```C#
DateTime fechaActual = DateTime.Now; // Obtiene la fecha actual
DateTime fechaFutura = fechaActual.AddDays(30); // Añade 30 días a la fecha actual
DateTime fechaPasada = fechaActual.AddMonths(-6); // Resta 6 meses a la fecha actual
```

Si necesitas calcular una fecha en específico, puedes crear un objeto de tipo `DateTime` con esa fecha en particular y luego utilizar los métodos mencionados anteriormente para realizar los cálculos necesarios. Además, puedes utilizar el método `ToString` para formatear la fecha resultante según tus preferencias. Por ejemplo:

```C#
DateTime fechaEspecifica = new DateTime(2021, 12, 25); // Crea una nueva fecha con el 25 de diciembre de 2021
Console.WriteLine(fechaEspecifica.AddYears(-1).ToString("MM/dd/yyyy")); // Resta un año y muestra la fecha en formato mes/día/año
```

### Resultado del código

La salida del código anterior sería:

```
11/25/2020
```

## Profundizando
En algunas situaciones, puede que necesites calcular fechas teniendo en cuenta días laborables o feriados. Para esto, puedes utilizar la clase `DateTime` y sus propiedades `DayOfWeek` y `DayOfYear` para determinar en qué día de la semana o del año cae la fecha en particular y ajustar el cálculo según sea necesario. También puedes utilizar la clase `CultureInfo` para considerar diferentes formatos de calendario en caso de que tu programa deba ser compatible con diferentes culturas.

## Ver también
- [Cómo trabajar con fechas y horas en C#](https://docs.microsoft.com/es-es/dotnet/standard/datetime/)
- [Cómo utilizar la clase DateTime en C#](https://www.c-sharpcorner.com/article/working-with-datetime-in-C-Sharp/)
- [Un vistazo a la clase CultureInfo en C#](https://www.c-sharpcorner.com/article/working-with-the-cultureinfo-class-in-C-Sharp/)