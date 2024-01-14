---
title:                "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

¡Hola lectores! En este blog post, vamos a hablar sobre cómo obtener la fecha actual en C#. Si eres un programador principiante o simplemente quieres refrescar tus conocimientos, ¡sigue leyendo para descubrir cómo puedes obtener la fecha actual en tu código!

## ¿Por qué? 

Puede parecer simple obtener la fecha actual, pero es una función importante en cualquier programa. Ya sea que estés creando una aplicación de gestión de tareas, un calendario o un juego, a menudo necesitarás mostrar la fecha actual al usuario. Además, obtener la fecha actual también puede ser útil para realizar cálculos y crear registros cronológicos.

## ¿Cómo hacerlo?

En C#, hay una clase predefinida llamada "DateTime" que te permite obtener la fecha y hora actuales. Para usar esta clase, primero debes agregar una referencia al espacio de nombres "System".

Una vez que hayas agregado la referencia, puedes crear una variable de tipo DateTime y asignarle la fecha y hora actuales utilizando el método "Now":

```C#
using System;

DateTime fechaActual = DateTime.Now;
```

Ahora que tienes la fecha actual almacenada en una variable, puedes mostrarla al usuario de la forma que prefieras. Por ejemplo, si quieres mostrar la fecha en formato día-mes-año, puedes usar el método "ToString" y especificar el formato deseado. Aquí hay un ejemplo:

```C#
Console.WriteLine(fechaActual.ToString("dd-MM-yyyy"));
```

Este código imprimirá la fecha actual en la consola en formato día-mes-año, como por ejemplo "06-05-2021".

## Buceo profundo

Además de obtener la fecha actual, la clase DateTime también tiene muchos otros métodos útiles para manipular fechas y horas. Puedes utilizar el método "Add" para sumar o restar una cantidad específica de días, horas, minutos, etc. a una fecha determinada. También puedes comparar dos fechas utilizando los métodos "Compare" o "Equals" y obtener la diferencia entre ellas usando el método "Subtract".

Es importante tener en cuenta que la fecha y hora actuales dependen de la zona horaria en la que se ejecuta el programa. Puedes especificar una zona horaria específica al utilizar el método "Now" y pasarle como parámetro un objeto de tipo "TimeZoneInfo".

¡Ahora que sabes cómo obtener la fecha actual en C#, puedes crear programas más dinámicos y útiles!

## Ver también

- [Documentación oficial de DateTime en Microsoft Docs](https://docs.microsoft.com/es-es/dotnet/api/system.datetime)
- [Tutorial sobre DateTime en C# en C# Corner](https://www.c-sharpcorner.com/article/date-time-in-C-Sharp/)
- [Ejemplos de uso de la clase DateTime en GeeksforGeeks](https://www.geeksforgeeks.org/datetime-class-in-C-Sharp/)

¡Gracias por leer y hasta la próxima!