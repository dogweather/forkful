---
title:                "Extraindo Substrings"
html_title:           "C#: Extraindo Substrings"
simple_title:         "Extraindo Substrings"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Extrair substrings é quando um programador pega uma parte de um texto maior e a separa em uma string separada. Os programadores fazem isso quando precisam manipular ou analisar apenas uma parte específica de uma string maior.

## Como fazer:
Para extrair substrings em C#, podemos usar o método `Substring()` da classe `string`. Veja o exemplo abaixo para extrair o sobrenome de um nome completo:

```C#
string nomeCompleto = "João da Silva";
string sobrenome = nomeCompleto.Substring(8);
Console.WriteLine(sobrenome);

// Output: da Silva
```

Também é possível especificar um índice de início e um comprimento para a substring desejada:

```C#
string text = "Hello World";
string substring = text.Substring(6, 5);
Console.WriteLine(substring);

//Output: World
```

## Mergulho profundo:
A extração de substrings é uma técnica comum e amplamente utilizada em programação, com muitas aplicações práticas, como análise de texto, manipulação de dados e processamento de linguagem natural. Antes do método `Substring()` estar disponível em C#, os programadores precisavam usar funções adicionais para manipular strings, o que era mais trabalhoso e propenso a erros.

Embora o método `Substring()` seja o mais comum, existem alternativas que podem ser usadas para extrair substrings em C#. Alguns programadores preferem a função `Substr()` do pacote `System.Linq` ou usar expressões regulares para encontrar e extrair substrings.

## Veja também:
- Documentação oficial do método `Substring()` em C#: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- Tutorial sobre expressões regulares em C#: https://www.c-sharpcorner.com/article/regex-in-c-sharp/
- Função `Substr()` da biblioteca `System.Linq`: https://docs.microsoft.com/en-us/dotnet/api/system.linq.substring?view=netcore-3.1