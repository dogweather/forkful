---
date: 2024-01-20 17:41:43.713822-07:00
description: 'Miten: .'
lastmod: '2024-03-13T22:44:56.557597-06:00'
model: gpt-4-1106-preview
summary: .
title: Merkkien poistaminen hakemalla osumia kaavaan
weight: 5
---

## Miten:
```C#
using System;
using System.Text.RegularExpressions;

class PatternDeletion
{
    static void Main()
    {
        string original = "Hei, maailma! 123.";
        string pattern = @"\d"; // Poistaa kaikki numerot
        string cleaned = Regex.Replace(original, pattern, "");

        Console.WriteLine(cleaned); // Output: Hei, maailma! .
    }
}
```

## Syväsukellus
Merkinmukaisen kuvion poistamisen historia juontaa juurensa varhaisiin tekstinkäsittelytarpeisiin. C#:ssa tämä on usein toteutettu säännöllisten lausekkeiden (Regular Expressions eli Regex) avulla, koska ne tarjoavat voimakkaan työkalun merkkijonojen käsittelyyn.

Vaihtoehtoja ovat LINQ-kyselyt tai jopa yksinkertainen merkkijonokäsittely `String.Replace`, mutta nämä menetelmät eivät ole yhtä joustavia kuin Regex. Tulee huomata, että Regex-käsittely voi olla suorituskyvyltään hitaampaa, joten kannattaa arvioida käyttötapaus huolellisesti.

## Katso Myös
- C# Regex-luokan dokumentaatio: [https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex)
- Microsoftin .NET -oppaat säännöllisistä lausekkeista: [https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- LINQ:n virallinen dokumentaatio: [https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/concepts/linq/)
