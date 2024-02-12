---
title:                "Pesquisando e substituindo texto"
aliases:
- /pt/c-sharp/searching-and-replacing-text/
date:                  2024-01-20T17:57:33.335540-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pesquisando e substituindo texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?
Procurar e substituir texto é uma forma de encontrar uma sequência específica de caracteres em uma string e trocá-la por outra. Programadores costumam fazer isso para corrigir erros, atualizar informações ou modificar dados de forma rápida e eficiente.

## Como Fazer:
```C#
using System;

class Program {
    static void Main() {
        string textoOriginal = "A raposa marrom ágil salta sobre o cão preguiçoso.";
        string textoSubstituido = textoOriginal.Replace("marrom", "vermelha");
        Console.WriteLine(textoSubstituido);

        // Usando Regex para substituição mais complexa
        using System.Text.RegularExpressions;

        string padrao = @"\bmarrom\b";
        string substituicao = "verde";
        string resultadoRegex = Regex.Replace(textoOriginal, padrao, substituicao);
        Console.WriteLine(resultadoRegex);
    }
}
```
Saída:
```
A raposa vermelha ágil salta sobre o cão preguiçoso.
A raposa verde ágil salta sobre o cão preguiçoso.
```

## Mergulho Profundo:
Desde os primeiros dias da computação, substituir texto foi essencial para editar códigos e conteúdo. No C#, `String.Replace()` é o método básico para isso, mas quando você precisa de mais flexibilidade, as expressões regulares (Regex) são as ferramentas certas. 

Alternativas como o método `StringBuilder.Replace()` podem ser úteis para strings muito grandes ou quando muitas substituições são necessárias, pois ele pode ser mais eficiente em termos de memória.

Quanto aos detalhes de implementação, `String.Replace()` funciona bem para substituições diretas e simples. Por outro lado, a classe `Regex` lida com padrões complexos que podem incluir caracteres curinga, quantificadores e agrupamentos, oferecendo uma poderosa linguagem de pesquisa e substituição.

## Veja Também:
- [Documentação do método String.Replace](https://docs.microsoft.com/pt-br/dotnet/api/system.string.replace?view=net-7.0)
- [Expressões regulares em .NET](https://docs.microsoft.com/pt-br/dotnet/standard/base-types/regular-expressions)
- [Documentação do StringBuilder.Replace](https://docs.microsoft.com/pt-br/dotnet/api/system.text.stringbuilder.replace?view=net-7.0)
