---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:16:44.417085-07:00
description: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) C#:ssa ovat tehokas ty\xF6\
  kalu merkkijonojen sis\xE4ll\xE4 tapahtuvaan mallin tunnistamiseen, jonka avulla\
  \ ohjelmoijat voivat\u2026"
lastmod: '2024-03-13T22:44:56.563347-06:00'
model: gpt-4-0125-preview
summary: "S\xE4\xE4nn\xF6lliset lausekkeet (regex) C#:ssa ovat tehokas ty\xF6kalu\
  \ merkkijonojen sis\xE4ll\xE4 tapahtuvaan mallin tunnistamiseen, jonka avulla ohjelmoijat\
  \ voivat tehokkaasti etsi\xE4, korvata, jakaa tai poimia tietoja."
title: "S\xE4\xE4nn\xF6llisten lausekkeiden k\xE4ytt\xF6"
weight: 11
---

## Kuinka:


### Yksinkertainen mallin tunnistaminen
Tarkistaaksesi sisältääkö merkkijono tietyn mallin, voit käyttää `Regex.IsMatch`-metodia `System.Text.RegularExpressions`-nimiavaruudesta.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Hello, World!";
        string pattern = "World";
        bool containsPattern = Regex.IsMatch(sampleText, pattern);

        Console.WriteLine(containsPattern);  // Tuloste: True
    }
}
```

### Tietojen poimiminen
Tiedon poimiminen merkkijonosta käyttäen ryhmiä regexissä voidaan tehdä `Regex.Match`-metodilla.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Päivämäärä: 2023-04-12";
        string pattern = @"Päivämäärä: (\d{4})-(\d{2})-(\d{2})";
        Match match = Regex.Match(sampleText, pattern);

        if (match.Success)
        {
            Console.WriteLine($"Vuosi: {match.Groups[1].Value}");  // Tuloste: Vuosi: 2023
            Console.WriteLine($"Kuukausi: {match.Groups[2].Value}");  // Tuloste: Kuukausi: 04
            Console.WriteLine($"Päivä: {match.Groups[3].Value}");  // Tuloste: Päivä: 12
        }
    }
}
```

### Tekstin korvaaminen
`Regex.Replace`-metodin avulla voit korvata merkkijonossa olevan tekstin, joka vastaa tiettyä mallia.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "Vieraile Microsoftissa!";
        string pattern = "Microsoft";
        string replacement = "Google";

        string result = Regex.Replace(sampleText, pattern, replacement);

        Console.WriteLine(result);  // Tuloste: Vieraile Googlessa!
    }
}
```

### Merkkijonojen jakaminen
Voit jakaa merkkijonon taulukoksi perustuen regex-malliin käyttäen `Regex.Split`-metodia.

```csharp
using System;
using System.Text.RegularExpressions;

class Program
{
    static void Main()
    {
        string sampleText = "yksi,kaksi,kolme,neljä,viisi";
        string pattern = ",";

        string[] result = Regex.Split(sampleText, pattern);

        foreach (string item in result)
        {
            Console.WriteLine(item);
        }
        // Tuloste: 
        // yksi
        // kaksi
        // kolme
        // neljä
        // viisi
    }
}
```

### Kolmannen osapuolen kirjastojen käyttö
Vaikka .NET Framework tarjoaa laajat valmiudet säännöllisiin lausekkeisiin, on myös kolmannen osapuolen kirjastoja, kuten `PCRE.NET`, jotka tarjoavat Perl-yhteensopivia säännöllisiä lausekkeita (PCRE) C#:ssa. Tämä voi olla hyödyllistä, jos tarvitset ominaisuuksia tai syntaksia Perl:n regex-moottorista, jotka eivät ole saatavilla .NET:n toteutuksessa.

`PCRE.NET`:n käyttämiseksi asentaisit ensin sen NuGet-paketin, ja sen jälkeen voit käyttää sitä samankaltaisella tavalla kuin käytät natiivia .NET regex-luokkia.

```csharp
// Esimerkki käyttäen PCRE.NET tässä
// Huom: Kuvittele esimerkki, samankaltainen kuin yllä, räätälöity esittelemään ominaisuutta, joka on ainutlaatuinen PCRE.NET:lle.
```

Kun integroit kolmannen osapuolen kirjastoja säännöllisiin lausekkeisiin, konsultoi aina niiden dokumentaatiota yksityiskohtaisen käyttö- ja yhteensopivuustiedon saamiseksi.
