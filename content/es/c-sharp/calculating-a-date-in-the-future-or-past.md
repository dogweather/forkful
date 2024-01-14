---
title:                "C#: Calculando una fecha en el futuro o en el pasado."
simple_title:         "Calculando una fecha en el futuro o en el pasado."
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

¡Hola a todos los lectores casuales de programación en C#! Hoy, vamos a hablar sobre cómo calcular una fecha en el futuro o en el pasado en C#. Si eres nuevo en la programación o simplemente quieres mejorar tus habilidades en C#, sigue leyendo. ¡Empecemos!

## Por qué

Calcular una fecha en el futuro o en el pasado puede ser útil en muchas situaciones. Por ejemplo, si estás creando una aplicación de tareas o recordatorios, es posible que necesites mostrar una fecha en el futuro o en el pasado. También puedes necesitar esta función si estás creando una aplicación de seguimiento de tiempo o una aplicación de planificación de eventos. Sea cual sea el caso, aprender a calcular fechas en C# puede ser una habilidad muy útil.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en C#, primero debes entender cómo funciona el sistema de fechas en C#. En este lenguaje, las fechas se representan con un objeto de tipo DateTime. Este objeto tiene diferentes métodos y propiedades que te permitirán manipular fechas de forma sencilla.

Para calcular una fecha en el futuro o en el pasado, utilizaremos el método Add de la clase DateTime. Este método acepta un parámetro de tipo TimeSpan, que representa un intervalo de tiempo. Puedes especificar este intervalo utilizando diferentes unidades de medida como días, horas, minutos, etc.

Veamos un ejemplo de cómo calcular una fecha en el futuro y en el pasado utilizando el método Add:

```C#
// calcular una fecha en el futuro (3 semanas a partir de hoy)
DateTime fechaFutura = DateTime.Today.Add(TimeSpan.FromDays(21));
Console.WriteLine(fechaFutura); // output: 09/09/2021

// calcular una fecha en el pasado (1 año a partir de hoy)
DateTime fechaPasada = DateTime.Today.Add(TimeSpan.FromDays(-365));
Console.WriteLine(fechaPasada); // output: 26/08/2020
```

Como puedes ver en el código anterior, proporcionamos un intervalo de tiempo en días utilizando el método FromDays de la clase TimeSpan. También puedes utilizar otros métodos como FromHours, FromMinutes, FromSeconds, etc. para especificar diferentes unidades de tiempo.

## Profundizando un poco más

Ahora que conoces los conceptos básicos para calcular fechas en C#, puedes profundizar un poco más y aprender sobre otros métodos útiles para manipular fechas. Por ejemplo, puedes utilizar los métodos AddDays, AddHours, AddMinutes, etc. para agregar un número específico de días, horas, minutos, etc. a una fecha determinada.

También puedes utilizar el método ToString para formatear una fecha según tus necesidades. Por ejemplo, puedes mostrar solo la fecha sin la hora, o puedes mostrar solo el año de una fecha. Otra función útil es el método Compare, que te permite comparar dos fechas y determinar cuál es mayor o menor.

¡Eso es todo por hoy! Esperamos que este artículo te haya ayudado a comprender cómo calcular fechas en el futuro o en el pasado en C#. Si quieres seguir aprendiendo más sobre programación en C#, te recomendamos que consultes los siguientes enlaces:

## Ver también

- [Documentación oficial de C#](https://docs.microsoft.com/es-es/dotnet/csharp/)
- [Más información sobre la clase DateTime](https://docs.microsoft.com/es-es/dotnet/api/system.datetime?view=net-5.0)
- [Ejemplos prácticos de cómo utilizar el método Add](https://www.c-sharpcorner.com/blogs/how-to-use-add-method-on-datetime-in-c-sharp1)
- [Tutorial básico de C#](https://www.tutorialspoint.com/csharp/index.htm)

¡Hasta la próxima! Happy coding!