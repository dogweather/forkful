---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar fechas es una tarea común en la programación que nos permite realizar diversas operaciones, como calcular la duración entre dos eventos o verificar si una fecha se encuentra dentro de un intervalo de tiempo específico.

## Cómo

La comparación de fechas en C# es sencilla y se puede realizar utilizando los métodos proporcionados por la clase DateTime. A continuación, se presentan algunos ejemplos de código que muestran cómo comparar dos fechas y su posible salida:

```
// Comparación de igualdad
DateTime fecha1 = new DateTime(2021, 10, 1);
DateTime fecha2 = new DateTime(2021, 10, 1);
Console.WriteLine(fecha1 == fecha2); // Salida: True

// Comparación de desigualdad
DateTime fecha3 = new DateTime(2021, 10, 1);
DateTime fecha4 = new DateTime(2021, 10, 2);
Console.WriteLine(fecha3 != fecha4); // Salida: True

// Comparación de mayor y menor
DateTime fecha5 = new DateTime(2021, 10, 1);
DateTime fecha6 = new DateTime(2021, 9, 30);
Console.WriteLine(fecha5 < fecha6); // Salida: False
Console.WriteLine(fecha5 > fecha6); // Salida: True
```

En estos ejemplos, podemos ver que se utilizan operadores lógicos para comparar las fechas, como el operador de igualdad (==), el operador de desigualdad (!=) y los operadores mayor que (>) y menor que (<).

También es posible utilizar los métodos proporcionados por la clase DateTime, como el método Equals() que devuelve un valor booleano indicando si dos fechas son iguales, el método Compare() que devuelve un entero indicando si una fecha es menor, igual o mayor que otra, y el método CompareTo() que devuelve un valor entero indicando si una fecha es menor, igual o mayor que otra.

## Deep Dive

Las fechas en C# se almacenan en formato numérico, donde el número representa la cantidad de ticks (unidades de tiempo) transcurridos desde el 1 de enero de 0001 a las 12:00 a.m. También es importante mencionar que C# permite trabajar con fechas en formatos personalizados, lo que puede ser útil en diferentes situaciones.

Además, al comparar fechas es importante tener en cuenta que también se comparan las horas, minutos, segundos y milisegundos, por lo que si no se especifican estos valores, la comparación puede no ser exacta. Para esto, es recomendable utilizar los métodos proporcionados para establecer y obtener los componentes de una fecha, como Year, Month, Day, Hour, Minute, Second y Millisecond.

## Ver también

- [Documentación oficial de Microsoft sobre la clase DateTime en C#](https://docs.microsoft.com/es-es/dotnet/api/system.datetime)
- [Tutorial de comparación de fechas en C#](https://www.c-sharpcorner.com/uploadfile/mahesh/compare-two-dates-using-C-Sharp)