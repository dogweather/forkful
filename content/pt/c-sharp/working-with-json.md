---
title:                "Trabalhando com json"
html_title:           "C#: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "C#"
category:             "C#"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/working-with-json.md"
---

{{< edit_this_page >}}

## Por que

JSON (JavaScript Object Notation) é um formato de dados extremamente popular e amplamente usado na programação moderna. Ao aprender a trabalhar com JSON, você será capaz de manipular e trocar dados de forma eficiente em uma ampla variedade de plataformas e aplicativos.

## Como fazer

Trabalhar com JSON em C# é muito simples e direto. Primeiro, você precisa importar a biblioteca System.Text.Json para o seu projeto. Em seguida, crie um objeto JSON usando as classes JsonDocument e JsonObject. Você pode, então, adicionar pares de chave e valor ao seu objeto usando o método Add. Por fim, você pode converter o objeto JSON em uma string usando o método ToString. Aqui está um exemplo de código para criar e exibir um objeto JSON simples:

```C#
using System;
using System.Text.Json;

class Program
{
    static void Main(string[] args)
    {
        // Cria o objeto JSON
        JsonDocument jsonObj = new JsonDocument();
        
        // Adiciona pares de chave e valor
        jsonObj.RootElement.Add("nome", "Maria");
        jsonObj.RootElement.Add("idade", 30);
        
        // Converte para string e exibe
        string jsonStr = jsonObj.ToString();
        Console.WriteLine(jsonStr);
    }
}

// Saída:
// {"nome":"Maria", "idade":30}
```

## Aprofundando-se

Além de apenas criar objetos JSON simples, também é possível analisar e acessar dados de objetos JSON existentes usando a classe JsonElement. Essa classe oferece métodos para navegar pelas propriedades e valores de um objeto JSON e extrair os dados desejados.

Também é importante entender a sintaxe do JSON, que é muito semelhante à sintaxe de objetos em C#. Isso torna o processo de conversão entre objetos C# e JSON mais intuitivo e fácil.

Outro recurso útil é a capacidade de serializar e desserializar objetos C# em JSON e vice-versa. Isso permite que você envie e receba dados JSON através de APIs e serviços da web de maneira eficiente.

## Veja também

Aqui estão alguns links para recursos adicionais sobre como trabalhar com JSON em C#:

- [Documentação oficial do JsonDocument](https://docs.microsoft.com/pt-br/dotnet/api/system.text.json.jsondocument)
- [Tutorial do Microsoft Docs sobre como trabalhar com JSON em C#](https://docs.microsoft.com/pt-br/dotnet/standard/serialization/system-text-json)
- [Artigo da C# Corner sobre como serializar e desserializar objetos em JSON](https://www.c-sharpcorner.com/article/serializing-and-deserializing-json-in-c-sharp/)