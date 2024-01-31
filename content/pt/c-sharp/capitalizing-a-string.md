---
title:                "Capitalizando uma string"
date:                  2024-01-19
simple_title:         "Capitalizando uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizar uma string significa transformar todas as letras iniciais das palavras em maiúsculas. Fazemos isso para garantir a consistência da formatação nos dados e melhorar a legibilidade, especialmente em títulos e nomes próprios.

## How to:
Para capitalizar uma string em C#, você pode usar o método `ToTitleCase()` da classe `TextInfo`, que faz parte da biblioteca `System.Globalization`. Aqui está um exemplo rápido:

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        TextInfo textInfo = new CultureInfo("pt-BR", false).TextInfo;
        string exemplo = "este é um exemplo de capitalização.";
        string resultado = textInfo.ToTitleCase(exemplo);
        
        Console.WriteLine(resultado); // Saída: Este É Um Exemplo De Capitalização.
    }
}
```

## Deep Dive
Antigamente, a capitalização em programação nem sempre foi tão direta, especialmente ao lidar com diferentes idiomas e convenções de escrita. No C#, o `ToTitleCase()` simplificou bastante o processo, mas ainda é importante entender suas nuances. Por exemplo, ele não alterará as palavras já em maiúsculas (para evitar mudanças em siglas, por exemplo).

Existem alternativas como o uso de expressões regulares (`Regex`) ou programação LINQ para manipular strings mais complexas. A implementação de uma função personalizada também é possível caso precise de uma lógica de capitalização específica.

## See Also
Para se aprofundar em manipulação de strings e outras funcionalidades da classe `TextInfo`, confira:

- Documentação da Microsoft sobre TextInfo: https://docs.microsoft.com/pt-br/dotnet/api/system.globalization.textinfo
- Guia sobre expressões regulares em C#: https://docs.microsoft.com/pt-br/dotnet/standard/base-types/regular-expressions
- Introdução ao LINQ em C#: https://docs.microsoft.com/pt-br/dotnet/standard/linq/
