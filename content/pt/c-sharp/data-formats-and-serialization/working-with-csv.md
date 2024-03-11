---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:33.425761-07:00
description: "CSV (Valores Separados por V\xEDrgula) s\xE3o arquivos de um formato\
  \ comum de troca de dados que representam dados tabulares em texto simples, usando\
  \ v\xEDrgulas\u2026"
lastmod: '2024-03-11T00:14:20.315720-06:00'
model: gpt-4-0125-preview
summary: "CSV (Valores Separados por V\xEDrgula) s\xE3o arquivos de um formato comum\
  \ de troca de dados que representam dados tabulares em texto simples, usando v\xED\
  rgulas\u2026"
title: Trabalhando com CSV
---

{{< edit_this_page >}}

## O que é & Por quê?
CSV (Valores Separados por Vírgula) são arquivos de um formato comum de troca de dados que representam dados tabulares em texto simples, usando vírgulas para separar os valores individuais. Programadores trabalham com arquivos CSV para importar, exportar e manipular dados com facilidade entre várias aplicações e serviços, já que é um formato simples, amplamente suportado e compatível com aplicações de planilhas, bancos de dados e linguagens de programação.

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
