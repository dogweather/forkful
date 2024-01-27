---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Isoilla kirjaimilla kirjoittaminen tarkoittaa merkkijonon muuttamista niin, että jokainen sana alkaa isolla alkukirjaimella. Ohjelmoijat käyttävät tätä parantaakseen tekstin ulkoasua tai noudattaakseen standardoituja muotoilusääntöjä.

## How to: (Kuinka tehdä:)

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string originalText = "finnish lake district";
        TextInfo textInfo = new CultureInfo("fi-FI", false).TextInfo;

        // Muuttaa jokaisen sanan alkamaan isolla kirjaimella
        string capitalizedText = textInfo.ToTitleCase(originalText);

        Console.WriteLine(capitalizedText);  // Tulostaa "Finnish Lake District"
    }
}
```

## Deep Dive (Syväsukellus)
Isojen kirjainten käyttöön alkoi nousta tarve, kun tekstinlukuohjelmat ja tietokoneet yleistyivät 1900-luvulla, ja tekstin selkeys muuttui tärkeäksi. Isoilla kirjaimilla alkavat sanat erotetaan paremmin toisistaan, ja se helpottaa lukemista.

Vaihtoehtoisesti C#:ssa voit käyttää LINQ-metodia tai kirjoittaa oman algoritmin stringin kapitalisointiin. `ToUpper()` ja `ToLower()`-metodeja voidaan myös soveltaa tietyn tyylin säilyttämiseksi, kuten akronyymien kanssa.

Kun ajatellaan toteutusta, `CultureInfo`-olion `TextInfo`-ominaisuus on tärkeä, koska se huomioi kulttuuriset säännöt, esimerkiksi erikoistapaukset tai poikkeukset.

## See Also (Katso Myös)

- Microsoftin virallinen `TextInfo.ToTitleCase` dokumentaatio: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo.totitlecase
- C# CultureInfo-luokan käyttö: https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo
- C# string-metodien kattava lista: https://docs.microsoft.com/en-us/dotnet/api/system.string
