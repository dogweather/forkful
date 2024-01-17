---
title:                "Obteniendo la fecha actual"
html_title:           "C#: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

¿Qué es y por qué los programadores lo hacen?

Obtener la fecha actual es una función común en la programación que nos permite obtener la fecha y hora actuales en un formato específico. Los programadores lo hacen para realizar tareas como registrar la hora de creación de un archivo o generar timestamps en aplicaciones y sitios web.

Cómo hacerlo:

```C#
// Obtener la fecha actual con el formato YYYY-MM-DD
DateTime fechaActual = DateTime.Now;
string fechaString = fechaActual.ToString("yyyy-MM-dd");

// Salida: 2021-09-09 
Console.WriteLine(fechaString);
```

```C#
// Obtener la fecha y hora actuales en un formato completo
DateTime fechaActual = DateTime.Now;
string fechaHoraString = fechaActual.ToString("dddd, dd MMMM yyyy HH:mm:ss");

// Salida: jueves, 09 septiembre 2021 10:00:00 
Console.WriteLine(fechaHoraString);
```

Profundizando:

- Contexto histórico: Obtener la fecha actual solía ser un proceso complicado y propenso a errores en los primeros días de la informática. Gracias a los avances tecnológicos y a los lenguajes de programación como C#, ahora es una tarea sencilla.
- Alternativas: Aparte de usar la función DateTime.Now en C#, también se puede obtener la fecha actual usando la clase Date en JavaScript o la función date en Python.
- Detalles de implementación: Además de los formatos de fecha y hora mencionados anteriormente, también se pueden especificar otros formatos como la hora UTC y el uso de formatos personalizados.

Ver también:

- [DateTime (C# Reference)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- [Obtener fecha y hora actuales en Python](https://www.programiz.com/python-programming/datetime/current-datetime)
- [Date (JavaScript)](https://developer.mozilla.org/es/docs/Web/JavaScript/Reference/Global_Objects/Date)