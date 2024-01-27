---
title:                "Trabalhando com JSON"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Trabalhar com JSON envolve manipular dados em um formato leve de troca, que é fácil para os humanos lerem e escreverem, e fácil para as máquinas analisarem e gerarem. Programadores usam JSON para interoperabilidade entre serviços de web, APIs e para armazenar configurações e dados de forma simples.

## Como Fazer:

Para manipular JSON em C#, a biblioteca mais comum é o `System.Text.Json`, introduzido no .NET Core 3.0. Aqui está como deserializar um JSON para um objeto e serializar um objeto de volta para JSON:

```C#
using System;
using System.Text.Json;

public class Produto
{
    public string Nome { get; set; }
    public decimal Preco { get; set; }
}

public class ExemploJson
{
    public static void Main()
    {
        string json = "{\"Nome\":\"Caneta\",\"Preco\":1.99}";

        // Deserializar o JSON para um objeto
        Produto produto = JsonSerializer.Deserialize<Produto>(json);
        Console.WriteLine($"Nome: {produto.Nome}, Preço: {produto.Preco}");

        // Serializar o objeto de volta para JSON
        string novoJson = JsonSerializer.Serialize(produto);
        Console.WriteLine(novoJson);
    }
}
```

Output:
```
Nome: Caneta, Preço: 1.99
{"Nome":"Caneta","Preco":1.99}
```

## Mergulho Profundo:

JSON, sigla de JavaScript Object Notation, foi introduzido por Douglas Crockford em 2001, ganhando popularidade como uma alternativa mais enxuta ao XML. Alternativas incluem YAML, um formato ainda mais orientado para a legibilidade humana, e Protocol Buffers, otimizado para a eficiência na comunicação máquina a máquina. No C#, além do `System.Text.Json`, existe a biblioteca `Newtonsoft.Json`, amplamente usada em versões anteriores do .NET. O `System.Text.Json` foi otimizado para melhor desempenho e utilização reduzida de memória, mas possivelmente menos recursos que `Newtonsoft.Json`.

## Veja Também:

- Documentação oficial do System.Text.Json: [https://docs.microsoft.com/pt-br/dotnet/standard/serialization/system-text-json-overview](https://docs.microsoft.com/pt-br/dotnet/standard/serialization/system-text-json-overview)
- Guia de migração de Newtonsoft.Json para System.Text.Json: [https://docs.microsoft.com/pt-br/dotnet/standard/serialization/system-text-json-migrate-from-newtonsoft-how-to](https://docs.microsoft.com/pt-br/dotnet/standard/serialization/system-text-json-migrate-from-newtonsoft-how-to)
- JSON.org, para uma exploração mais profunda do formato JSON: [https://www.json.org/json-pt.html](https://www.json.org/json-pt.html)
