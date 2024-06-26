---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:33.425761-07:00
description: "Como fazer: Trabalhar com arquivos CSV em C# pode ser realizado atrav\xE9\
  s do namespace `System.IO` para opera\xE7\xF5es b\xE1sicas, e para manipula\xE7\xF5\
  es mais\u2026"
lastmod: '2024-03-13T22:44:46.606215-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com arquivos CSV em C# pode ser realizado atrav\xE9s do namespace\
  \ `System.IO` para opera\xE7\xF5es b\xE1sicas, e para manipula\xE7\xF5es mais complexas\
  \ ou para lidar com arquivos maiores de forma fluente, pode-se considerar bibliotecas\
  \ de terceiros como `CsvHelper`."
title: Trabalhando com CSV
weight: 37
---

## Como fazer:
Trabalhar com arquivos CSV em C# pode ser realizado através do namespace `System.IO` para operações básicas, e para manipulações mais complexas ou para lidar com arquivos maiores de forma fluente, pode-se considerar bibliotecas de terceiros como `CsvHelper`. Abaixo estão exemplos de como ler e escrever em arquivos CSV usando ambas as abordagens.

### Lendo um arquivo CSV usando System.IO
```csharp
using System;
using System.IO;

class ReadCSV
{
    static void Main()
    {
        string filePath = @"caminho\para\seu\arquivo.csv";
        // Lendo todas as linhas do arquivo CSV
        string[] csvLines = File.ReadAllLines(filePath);
        
        foreach (string line in csvLines)
        {
            string[] rowData = line.Split(',');
            Console.WriteLine($"Primeira Coluna: {rowData[0]}, Segunda Coluna: {rowData[1]}");
        }
    }
}
```

**Saída do exemplo:**
```
Primeira Coluna: Nome, Segunda Coluna: Idade
Primeira Coluna: John Doe, Segunda Coluna: 30
```

### Escrevendo em um arquivo CSV usando System.IO
```csharp
using System;
using System.Collections.Generic;
using System.IO;

class WriteCSV
{
    static void Main()
    {
        string filePath = @"caminho\para\seu\saida.csv";
        var lines = new List<string>
        {
            "Nome,Idade",
            "John Doe,30",
            "Jane Smith,25"
        };
        
        File.WriteAllLines(filePath, lines);
        Console.WriteLine("Arquivo CSV escrito.");
    }
}
```

**Saída do exemplo:**
```
Arquivo CSV escrito.
```

### Usando CsvHelper para Ler CSV
Para usar CsvHelper, primeiro, adicione o pacote `CsvHelper` ao seu projeto usando o NuGet Package Manager.

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
        string filePath = @"caminho\para\seu\arquivo.csv";

        using (var reader = new StreamReader(filePath))
        using (var csv = new CsvReader(reader, CultureInfo.InvariantCulture))
        {
            var records = csv.GetRecords<dynamic>().ToList();
            foreach (var record in records)
            {
                Console.WriteLine($"Primeira Coluna: {record.Name}, Segunda Coluna: {record.Age}");
            }
        }
    }
}
```

**Saída do exemplo:**
```
Primeira Coluna: John Doe, Segunda Coluna: 30
Primeira Coluna: Jane Smith, Segunda Coluna: 25
```

### Usando CsvHelper para Escrever CSV
```csharp
using CsvHelper;
using System.Globalization;
using System.IO;
using System.Collections.Generic;
using CsvHelper.Configuration;

class WriteCSVWithCsvHelper
{
    public class Pessoa
    {
        public string Nome { get; set; }
        public int Idade { get; set; }
    }

    static void Main()
    {
        string filePath = @"caminho\para\seu\saida.csv";
        var registros = new List<Pessoa>
        {
            new Pessoa { Nome = "John Doe", Idade = 30 },
            new Pessoa { Nome = "Jane Smith", Idade = 25 }
        };

        using (var writer = new StreamWriter(filePath))
        using (var csv = new CsvWriter(writer, CultureInfo.InvariantCulture))
        {
            csv.WriteRecords(registros);
        }
        
        Console.WriteLine("Arquivo CSV escrito com CsvHelper.");
    }
}
```

**Saída do exemplo:**
```
Arquivo CSV escrito com CsvHelper.
```
