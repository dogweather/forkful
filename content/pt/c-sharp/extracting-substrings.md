---
title:    "C#: Extraindo Substrings"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que extrair substrings?

Extrair substrings é uma técnica comum em programação que envolve a obtenção de uma parte específica de uma string. Isso pode ser útil em uma variedade de situações, como manipulação de dados, validação de entradas e formatação de saída.

## Como fazer:

Extrair substrings em C# é fácil e pode ser feito usando o método `Substring()`. Este método aceita dois argumentos: o índice inicial da substring desejada e o comprimento da substring. Por exemplo, para extrair a segunda e terceira letra de uma string "exemplo", podemos usar o seguinte código:

```C#
string exemplo = "exemplo";
string substring = exemplo.Substring(1, 2);
Console.WriteLine(substring);
```

Isso resultaria em "xe" sendo impresso na tela. Lembre-se de que o índice inicial começa em 0, então o primeiro caractere seria o índice 0.

## Aprofundando:

Além do método `Substring()`, C# também oferece outras formas de extrair substrings, como o método `Split()`, que é útil para separar strings com base em um caractere específico. Também é importante entender como os índices funcionam em C#, pois eles podem ser usados ​​para extrair substrings de maneiras diferentes e complexas.

## Veja também:

- [Documentação do método `Substring()` em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.string.substring)
- [Tutorial sobre manipulação de strings em C#](https://www.tutorialspoint.com/csharp/csharp_strings.htm)
- [Artigo sobre índices e intervalos em C#](https://www.c-sharpcorner.com/article/c-sharp-7-indices-and-range-expressions/)