---
title:                "Descobrindo o comprimento de uma string"
date:                  2024-01-20T17:47:19.241124-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Saber o comprimento de uma string é descobrir o número de caracteres que ela contém. Programadores fazem isso para validar entradas, limitar conteúdo, ou simplesmente para manipular dados de forma precisa.

## How to:
Em C#, você consegue o tamanho de uma string usando a propriedade `Length`. Veja alguns exemplos:

```C#
string saudacao = "Olá, mundo!";
int tamanho = saudacao.Length;
Console.WriteLine(tamanho); // Saída: 12
```

Se você quiser contar somente letras ou dígitos, pode usar LINQ:

```C#
string frase = "Olá, mundo! 123";
int letras = frase.Count(char.IsLetter);
Console.WriteLine(letras); // Saída: 10
int digitos = frase.Count(char.IsDigit);
Console.WriteLine(digitos); // Saída: 3
```

## Deep Dive
Historicamente, a propriedade `Length` tem sido a forma padrão de se obter o tamanho de uma string em C#. Alternativas incluem métodos como `StringInfo.LengthInTextElements`, útil para strings com caracteres compostos ou emojis. Esse método conta elementos de texto conforme as regras do Unicode em vez de caracteres individuais. 

Internamente, a string em C# é implementada como um array de caracteres Unicode (UTF-16). Portanto, `Length` retorna o número de `Char` objetos no array, o que não necessariamente corresponde ao número de pontos de código Unicode quando há caracteres compostos.

```C#
string emojiString = "👩‍👩‍👧‍👦";
Console.WriteLine(emojiString.Length); // Saída: 11 (não reflete elementos de texto real)
```

Para um tratamento mais preciso de strings complexas, você poderia usar:

```C#
int textElementCount = new StringInfo(emojiString).LengthInTextElements;
Console.WriteLine(textElementCount); // Saída: 1
```

Além do C#, outras linguagens de programação têm suas próprias maneiras de lidar com o comprimento de strings, muitas vezes levando em conta especificidades culturais e de linguagem, como caracteres chineses, que podem mudar a abordagem para contagem e manipulação.

## See Also
- Microsoft Docs sobre a propriedade `Length`: [Propriedade Length](https://docs.microsoft.com/en-us/dotnet/api/system.string.length?view=net-7.0)
- Informações Unicode da Microsoft Docs: [StringInfo.LengthInTextElements](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.stringinfo.lengthintextelements?view=net-7.0)
- Exemplo de LINQ no Stack Overflow: [Contagem de caracteres utilizando LINQ](https://stackoverflow.com/questions/5489987/linq-to-count-characters-in-a-string)
- Tutorial do Unicode: [Como contar caracteres em strings](https://unicode.org/reports/tr29/#Grapheme_Cluster_Boundaries)
