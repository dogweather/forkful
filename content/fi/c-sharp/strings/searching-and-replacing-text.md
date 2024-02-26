---
date: 2024-01-20 17:57:59.228887-07:00
description: "Ohjelmoinnissa tekstinhaku ja -korvaus tarkoittaa merkkijonon etsimist\xE4\
  \ ja sen korvaamista toisella. Sit\xE4 k\xE4ytet\xE4\xE4n tiedon muokkaukseen ja\u2026"
lastmod: '2024-02-25T18:49:53.473690-07:00'
model: gpt-4-1106-preview
summary: "Ohjelmoinnissa tekstinhaku ja -korvaus tarkoittaa merkkijonon etsimist\xE4\
  \ ja sen korvaamista toisella. Sit\xE4 k\xE4ytet\xE4\xE4n tiedon muokkaukseen ja\u2026"
title: Tekstin etsiminen ja korvaaminen
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Ohjelmoinnissa tekstinhaku ja -korvaus tarkoittaa merkkijonon etsimistä ja sen korvaamista toisella. Sitä käytetään tiedon muokkaukseen ja automatisointiin, esimerkiksi korjattaessa bugeja tai päivitettäessä tietoja.

## How to (Kuinka tehdä):
```C#
using System;
using System.Text.RegularExpressions;

class Program {
    static void Main() {
        string sourceText = "Hello, World!";
        string pattern = "World";
        string replacement = "Finland";

        // Perus string.Replace
        string result = sourceText.Replace(pattern, replacement);
        Console.WriteLine(result); // Output: Hello, Finland!

        // Regex.Replace monimutkaisempia toimintoja varten
        pattern = @"\bWorld\b";
        result = Regex.Replace(sourceText, pattern, replacement);
        Console.WriteLine(result); // Output: Hello, Finland!
    }
}
```

## Deep Dive (Syväsukellus):
Tekstinkorjailu on vanha juttu, alkaen paperilta kynällä tehdystä korjaamisesta aina tietokoneiden tekstinkäsittelyohjelmiin. C#:ssa `String.Replace()` on suoraviivainen tapa korvata tekstiä, kun taas `Regex.Replace()` antaa voimaa mallintaa monimutkaisempia kaavoja ja ehtoja.

Vaihtoehtoisia metodeja ovat StringBuilder-luokka ja LINQ-operaatiot, jos suorituskyky on tärkeää tai data on kompleksisempaa. Toteutuksessa kannattaa huomioida myös merkistöjen (kuten UTF-8) ja kulttuurillisten erojen (kuten erilaiset aakkostot) vaikutus hakuun ja korvaukseen.

## See Also (Katso myös):
- Microsoft Docs: [String.Replace Method](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=net-6.0)
- Microsoft Docs: [Regex.Replace Method](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex.replace?view=net-6.0)
- Stack Overflow: [When to use StringBuilder?](https://stackoverflow.com/questions/3069416/whats-the-main-difference-between-string-and-stringbuilder)
- CodeProject: [Manipulating Strings in C#](https://www.codeproject.com/Articles/1014073/Manipulating-strings-in-Csharp)
