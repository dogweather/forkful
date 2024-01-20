---
title:                "Capitalizando uma string"
html_title:           "C#: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Capitalizando uma String em C#

## O que & Porquê?

Capitalizar uma string refere-se a transformar o primeiro caractere de cada palavra em maiúscula. Os programadores fazem isso para melhorar a legibilidade e aparência dos dados de texto.

## Como fazer:

Em C#, temos o método ToTitleCase em TextInfo que é uma parte do sistema.Globalization namespace. Aqui está um exemplo básico:

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string minhaString = "olá mundo!";
        TextInfo myTI = new CultureInfo("en-US", false).TextInfo;
        minhaString = myTI.ToTitleCase(minhaString);
        Console.WriteLine(minhaString); // Saída: Olá Mundo!
    }
}
```

## Mergulho Profundo

O método ToTitleCase tem raízes na criação do .NET Framework, fazendo parte da classe TextInfo. Como alternativa, os programadores podem usar a combinação de métodos ToLower() e ToUpper(), mas isso é mais demorado.

Sobre a implementação, ToTitleCase itera sobre cada caractere da string, transformando-o para maiúsculo se é o primeiro caractere de uma palavra, e para minúsculo caso contrário.

```C#
public string ToTitleCase(string str) 
{
    string[] words = str.Split(' ');
    for (int i = 0; i < words.Length; i++)
    {
        if (words[i].Length > 0)
        {
            char firstLetter = char.ToUpper(words[i][0]);
            string restOfWord = words[i].Substring(1).ToLower();
            words[i] = firstLetter + restOfWord;
        }
    }
    return string.Join(' ', words);
}
```
Note que este método não funciona perfeitamente com palavras conectadas por hífens ou apóstrofos.

## Veja Também

1. [Método ToTitleCase](https://docs.microsoft.com/pt-br/dotnet/api/system.globalization.textinfo.totitlecase)
2. [Namespace System.Globalization](https://docs.microsoft.com/pt-br/dotnet/api/system.globalization)
3. [Métodos de String em C#](https://docs.microsoft.com/pt-br/dotnet/api/system.string)
4. [C# Programação](https://www.tutorialsteacher.com/csharp/csharp-tutorials)