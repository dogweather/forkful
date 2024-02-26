---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:16.570883-07:00
description: "Los archivos CSV (Valores Separados por Comas) son un formato com\xFA\
  n de intercambio de datos que representa datos tabulares en texto plano, utilizando\u2026"
lastmod: '2024-02-25T18:49:55.570226-07:00'
model: gpt-4-0125-preview
summary: "Los archivos CSV (Valores Separados por Comas) son un formato com\xFAn de\
  \ intercambio de datos que representa datos tabulares en texto plano, utilizando\u2026"
title: Trabajando con CSV
---

{{< edit_this_page >}}

## Qué y Por Qué?
Los archivos CSV (Valores Separados por Comas) son un formato común de intercambio de datos que representa datos tabulares en texto plano, utilizando comas para separar los valores individuales. Los programadores trabajan con archivos CSV para importar, exportar y manipular datos con facilidad a través de varias aplicaciones y servicios, ya que es un formato simple, ampliamente soportado y compatible con aplicaciones de hojas de cálculo, bases de datos y lenguajes de programación.

## Cómo hacerlo:
Trabajar con archivos CSV en C# se puede lograr a través del espacio de nombres `System.IO` para operaciones básicas, y para manipulaciones más complejas o para manejar archivos más grandes sin problemas, se podría considerar bibliotecas de terceros como `CsvHelper`. A continuación, se muestran ejemplos de cómo leer y escribir en archivos CSV utilizando ambos enfoques.

### Leyendo un archivo CSV usando System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"ruta\al\tu\archivo.csv";
        // Leyendo todas las líneas del archivo CSV
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"Primera Columna: {rowData[0]}, Segunda Columna: {rowData[1]}");
        }
    }
}
```

**Salida de muestra:**
```
Primera Columna: Nombre, Segunda Columna: Edad
Primera Columna: John Doe, Segunda Columna: 30
```

### Escribiendo en un archivo CSV usando System.IO
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"ruta\al\tu\salida.csv";
        var lines = new List<string>
        {
            "Nombre,Edad",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("Archivo CSV escrito.");
    }
}
```

**Salida de muestra:**
```
Archivo CSV escrito.
```

### Usando CsvHelper para Leer CSV
Para usar CsvHelper, primero, agrega el paquete `CsvHelper` a tu proyecto usando el Gestor de Paquetes NuGet.

```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Linq;
using CsvHelper.Configuration;

class ReadCSVWithCsvHelper
{
    static void Main()
    {
        string filePath = @"ruta\al\tu\archivo.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"Primera Columna: {record.Name}, Segunda Columna: {record.Age}");
            }
        }
    }
}
```

**Salida de muestra:**
```
Primera Columna: John Doe, Segunda Columna: 30
Primera Columna: Jane Smith, Segunda Columna: 25
```

### Usando CsvHelper para Escribir CSV
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class WriteCSVWithCsvHelper
{
    public class Persona
    {
        public string Name { get; set; }
        public int Age { get; set; }
    }

    static void Main()
    {
        string filePath = @"ruta\al\tu\salida.csv";
        var records = new List<Persona>
        {
            new Persona { Name = "John Doe", Age = 30 },
            new Persona { Name = "Jane Smith", Age = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(records);
        }
        
        Console.WriteLine("Archivo CSV escrito con CsvHelper.");
    }
}
```

**Salida de muestra:**
```
Archivo CSV escrito con CsvHelper.
```
