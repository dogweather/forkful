---
title:                "Convertendo uma string para minúsculas"
html_title:           "C#: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

É comum usar strings em programas de computador para armazenar e manipular informações. A conversão de uma string para letras minúsculas pode ser útil em várias situações, desde comparações de texto até formatação de dados de entrada.

## Como fazer

Para converter uma string para letras minúsculas em C#, podemos usar o método `ToLower()` da classe `String`. Veja um exemplo abaixo:
```C#
string texto = "Olá, MUNDO!";
string textoMin = texto.ToLower();
Console.WriteLine(textoMin);
```
O output será: `olá, mundo!` 

## Mergulho profundo

Ao trabalhar com strings em C#, é importante entender como esse tipo de dado é tratado pelo sistema. Uma string é imutável, ou seja, quando fazemos alguma alteração nela, na verdade estamos criando uma nova string com as modificações. Por isso, a conversão de uma string para letras minúsculas não muda a string original, mas sim cria uma nova string com o texto em letras minúsculas.

Também é importante ter cuidado com os caracteres acentuados ao converter uma string para letras minúsculas. Em alguns casos, ao usar o método `ToLower()`, a acentuação pode ser removida ou alterada, dependendo da versão do C# e do sistema operacional. Para contornar esse problema, podemos usar o método `ToLowerInvariant()`, que garante que a conversão será feita de forma consistente em qualquer plataforma.

## Veja também

- [Método ToLower() na documentação oficial da Microsoft](https://docs.microsoft.com/pt-br/dotnet/api/system.string.tolower)
- [Diferenças entre ToLower() e ToLowerInvariant()](https://stackoverflow.com/questions/8353342/difference-between-string-tolower-and-string-tolowerinvariant)