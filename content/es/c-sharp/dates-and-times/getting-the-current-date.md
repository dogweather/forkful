---
title:                "Obteniendo la fecha actual"
aliases:
- /es/c-sharp/getting-the-current-date/
date:                  2024-02-03T19:09:08.547492-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obteniendo la fecha actual"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Obtener la fecha actual en C# implica obtener los detalles actuales de fecha y hora del sistema. Los programadores a menudo necesitan acceder a esta información para el registro, el sellado de tiempo de operaciones, o la programación de tareas dentro de aplicaciones, asegurando que las acciones estén sincronizadas con precisión y los datos estén marcados con sellos de tiempo precisos.

## Cómo hacerlo:
C# ofrece una manera directa de obtener la fecha actual usando la clase `DateTime`, que es parte del espacio de nombres System del .NET Framework. El ejemplo a continuación demuestra cómo obtener la fecha actual y, opcionalmente, la hora.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Obtiene solo la fecha actual
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Salida: MM/dd/yyyy
        
        // Obtiene la fecha y hora actuales
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Salida: MM/dd/yyyy HH:mm:ss

        // Obtiene la fecha y hora actuales UTC
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Salida: MM/dd/yyyy HH:mm:ss
    }
}
```

En términos de bibliotecas de terceros, NodaTime ofrece una alternativa robusta para la manipulación de fechas y horas, incluyendo la obtención de la fecha actual en diferentes calendarios y zonas horarias.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Usando NodaTime para obtener la fecha actual en el calendario ISO
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Salida: yyyy-MM-dd

        // Para fechas específicas de zona horaria
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Salida: yyyy-MM-dd
    }
}
```

Esto muestra el uso básico con la clase `DateTime` incorporada y las capacidades mejoradas proporcionadas por NodaTime, especialmente útiles para aplicaciones que requieren el manejo de diferentes zonas horarias o sistemas de calendario.
