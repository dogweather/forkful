---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Trabalhar com CSV (valores separados por vírgulas) significa manipular dados em um formato de texto simples que é amplamente utilizado para importação e exportação de informações entre bancos de dados e aplicativos. Programadores utilizam CSV devido à sua simplicidade e compatibilidade universal.

## Como fazer:

Para ler e escrever em arquivos CSV em C#, você pode usar a biblioteca `TextFieldParser` ou `StreamWriter`. Aqui está um exemplo de cada:

```C#
// Lendo CSV com 'TextFieldParser'
using Microsoft.VisualBasic.FileIO; // Você precisa adicionar referência
using System;

namespace CSVExample
{
    class Program
    {
        static void Main(string[] args)
        {
            using(TextFieldParser csvParser = new TextFieldParser(@"caminho_do_arquivo.csv"))
            {
                csvParser.CommentTokens = new string[] { "#" };
                csvParser.SetDelimiters(new string[] { "," });
                csvParser.HasFieldsEnclosedInQuotes = true;

                while (!csvParser.EndOfData)
                {
                    string[] fields = csvParser.ReadFields();
                    Console.WriteLine($"Nome: {fields[0]}, Idade: {fields[1]}");
                }
            }
        }
    }
}

```

```C#
// Escrevendo em CSV com 'StreamWriter'
using System;
using System.IO;

namespace CSVExample
{
    class Program
    {
        static void Main(string[] args)
        {
            string[] lines = {
                "Nome,Idade",
                "Alice,23",
                "Bob,30"
            };

            using (StreamWriter sw = new StreamWriter(@"caminho_do_arquivo.csv"))
            {
                foreach (string line in lines)
                {
                    sw.WriteLine(line);
                }
            }
        }
    }
}
```

Saída de exemplo ao ler um arquivo CSV:
```
Nome: Alice, Idade: 23
Nome: Bob, Idade: 30
```

## Aprofundando

Historicamente, o formato CSV é valorizado pela sua simplicidade, porém a falta de um padrão estrito pode causar problemas de interoperabilidade. Alternativas como o formato JSON ou o XML são opções com estruturas mais rígidas. A implementação em C# pode ser otimizada com bibliotecas de terceiros como `CsvHelper`, que oferece funcionalidades avançadas e melhor performance na leitura e escrita de arquivos CSV.

## Veja Também

- Documentação da Microsoft sobre o `TextFieldParser`: [TextFieldParser Class](https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualbasic.fileio.textfieldparser)
- Biblioteca `CsvHelper`: [CsvHelper GitHub](https://github.com/JoshClose/CsvHelper)
