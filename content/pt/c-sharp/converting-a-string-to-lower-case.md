---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Converter uma string para caixa baixa em C# significa transformar todas as letras maiúsculas em minúsculas. Programadores fazem isso para padronizar os dados, garantindo a precisão de comparações e buscas.

## Como fazê-lo:
Aqui está um exemplo simples:
```C#
string strOriginal = "Fala DEV!";
string strMinusc = strOriginal.ToLower();
Console.WriteLine(strMinusc);
```
Como resultado veremos: `fala dev!`.

## Deep Dive:
- **Contexto Histórico**: A função de conversão de string para minúsculas tem sido padrão nos idiomas de programação desde a origem do C.
- **Alternativas**: Se você quiser converter apenas uma letra específica, pode fazê-lo, em vez de converter a string inteira.
```C#
char charMaiusc = 'D', charMinusc;
charMinusc = char.ToLower(charMaiusc);
Console.WriteLine(charMinusc);
```
Resultado: `d`.
- **Detalhes de implementação**: O código `ToLower` leva em conta as especificidades linguísticas, ou seja, funciona mesmo com caracteres especiais do português como "Ç", "Ã", etc.

## Ver também:
- Documentação Microsoft sobre `ToLower`: https://docs.microsoft.com/pt-br/dotnet/api/system.string.tolower?view=net-5.0
- Comparando strings em C#: https://docs.microsoft.com/pt-br/dotnet/csharp/how-to/compare-strings
- Caracteres Unicode em C#: https://docs.microsoft.com/pt-br/dotnet/csharp/programming-guide/strings/how-to-use-unicode-characters-in-strings