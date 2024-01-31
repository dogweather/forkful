---
title:                "Trabajando con archivos CSV"
date:                  2024-01-19
simple_title:         "Trabajando con archivos CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Trabajar con archivos CSV (Valores Separados por Comas) implica leer, escribir y analizar datos estructurados en forma de texto, típicamente exportados o importados desde hojas de cálculo o bases de datos. Los programadores gestionan archivos CSV porque son fáciles de entender, intercambiar y funcionan en muchos sistemas y aplicaciones.

## Cómo hacerlo:
Para manejar archivos CSV en C#, puedes usar las siguientes estrategias. Aquí un ejemplo de cómo leer y escribir un archivo CSV:

```C#
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class Program
{
    static void Main()
    {
        var csvData = "Nombre,Edad,Ciudad\nJuan,34,Madrid\nLucia,29,Barcelona";

        // Escribir en archivo CSV
        File.WriteAllText("datos.csv", csvData);

        // Leer desde archivo CSV
        var lines = File.ReadAllLines("datos.csv");
        var csvValues = from line in lines
                        let data = line.Split(',')
                        select new { Nombre = data[0], Edad = data[1], Ciudad = data[2] };

        // Imprimir datos
        foreach (var item in csvValues)
        {
            Console.WriteLine($"Nombre: {item.Nombre}, Edad: {item.Edad}, Ciudad: {item.Ciudad}");
        }
    }
}
```

Output:
```
Nombre: Nombre, Edad: Edad, Ciudad: Ciudad
Nombre: Juan, Edad: 34, Ciudad: Madrid
Nombre: Lucia, Edad: 29, Ciudad: Barcelona
```

## Análisis Profundo:
Los CSV existen desde hace décadas como un estándar informal para intercambiar datos. Alternativas más modernas serían JSON o XML, aunque los CSV son insuperables en simplicidad y legibilidad por humanos y sistemas. Técnicamente, al implementar una solución para CSV en C#, asegúrate de manejar correctamente varios casos de borde como valores que incluyan comas, comillas dobles, o saltos de línea.

## Ver También:
- Documentación oficial de [`File`](https://docs.microsoft.com/en-us/dotnet/api/system.io.file?view=net-7.0) y [`File.ReadAllLines`](https://docs.microsoft.com/en-us/dotnet/api/system.io.file.readalllines?view=net-7.0) en C#.
- Un parser de CSV popular para C#: [CsvHelper](https://joshclose.github.io/CsvHelper/).
- RFC 4180, el estándar oficial para archivos CSV: [RFC 4180](https://www.ietf.org/rfc/rfc4180.txt).
