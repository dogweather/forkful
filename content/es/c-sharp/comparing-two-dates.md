---
title:                "C#: Comparando dos fechas"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Comparar dos fechas es una tarea común en la programación. Puede ser útil para realizar cálculos basados en fechas o para determinar si un evento ha ocurrido antes o después de otro. En esta publicación, aprenderemos cómo comparar dos fechas en C#.

## Cómo hacerlo

El método más sencillo para comparar dos fechas en C# es utilizar el operador de comparación "menor que" o "mayor que". Esto funciona porque las fechas en C#, en realidad, son números, por lo que pueden ser comparadas como tales.

```C#
DateTime fecha1 = new DateTime(2020, 10, 15);
DateTime fecha2 = new DateTime(2021, 01, 01);

if(fecha1 < fecha2)
{
    Console.WriteLine("La fecha 1 es anterior a la fecha 2.");
}
else if (fecha1 > fecha2)
{
    Console.WriteLine("La fecha 1 es posterior a la fecha 2.");
}
```

En este ejemplo, creamos dos objetos DateTime, uno para cada fecha que queremos comparar. Luego, utilizamos un condicional if-else para determinar si la fecha 1 es anterior o posterior a la fecha 2.

Otra forma de comparar fechas en C# es utilizando el método `Compare()` de la clase `DateTime`. Este método devuelve un número entero que indica si la primera fecha es menor, igual o mayor que la segunda fecha.

```C#
DateTime fecha1 = new DateTime(2020, 10, 15);
DateTime fecha2 = new DateTime(2021, 01, 01);

if(DateTime.Compare(fecha1, fecha2) < 0)
{
    Console.WriteLine("La fecha 1 es anterior a la fecha 2.");
}
else if (DateTime.Compare(fecha1, fecha2) > 0)
{
    Console.WriteLine("La fecha 1 es posterior a la fecha 2.");
}
```

En este ejemplo, utilizamos el método `Compare()` para comparar las fechas y luego utilizamos un condicional if-else para imprimir el mensaje correspondiente.

## Profundizando

Comparar dos fechas no solo se limita a determinar si una es mayor o menor que la otra. También podemos realizar comparaciones más específicas, como si las fechas son iguales o si son del mismo año, mes o día.

Para comparar si dos fechas son iguales, podemos utilizar el método `Equals()` de la clase `DateTime`.

```C#
// Supongamos que tenemos la siguiente fecha:
DateTime fecha1 = new DateTime(2020, 10, 15);

DateTime fecha2 = new DateTime(2020, 10, 15);

if(fecha1.Equals(fecha2))
{
    Console.WriteLine("Las fechas son iguales.");
}
```

Si queremos comparar solo el año, el mes o el día de dos fechas, podemos utilizar las propiedades `Year`, `Month` y `Day` de la clase `DateTime`.

```C#
DateTime fecha1 = new DateTime(2020, 10, 15);
DateTime fecha2 = new DateTime(2021, 01, 01);

if(fecha1.Year == fecha2.Year)
{
    Console.WriteLine("Las fechas tienen el mismo año.");
}

if(fecha1.Month == fecha2.Month)
{
    Console.WriteLine("Las fechas tienen el mismo mes.");
}

if(fecha1.Day == fecha2.Day)
{
    Console.WriteLine("Las fechas tienen el mismo día.");
}
```

Ten en cuenta que también podemos utilizar los operadores de igualdad (`==`) y desigualdad (`!=`) para realizar estas comparaciones.

## Ver también

- Documentación de Microsoft sobre la clase `DateTime`: https://docs.microsoft.com/es-es/dotnet/api/system.datetime?view=net-5.0
- Ejemplos de comparaciones de fechas en C#: https://www.c-sharpcorner.com/article/comparing-dates-and-datetime-is-so-easy-in-net/