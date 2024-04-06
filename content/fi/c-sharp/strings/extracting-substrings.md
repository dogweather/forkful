---
date: 2024-01-20 17:45:32.722945-07:00
description: "How to: (Kuinka tehd\xE4:) C# tarjoaa useita tapoja ty\xF6st\xE4\xE4\
  \ substringeja. Tutkitaanpa muutama esimerkki. 1. `Substring`-metodi."
lastmod: '2024-04-05T21:53:58.129636-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4:) C# tarjoaa useita tapoja ty\xF6st\xE4\xE4 substringeja."
title: Merkkijonojen osien poimiminen
weight: 6
---

## How to: (Kuinka tehdä:)
C# tarjoaa useita tapoja työstää substringeja. Tutkitaanpa muutama esimerkki.

1. `Substring`-metodi:

```C#
string esimerkki = "Hello, World!";
string tervehdys = esimerkki.Substring(0, 5);
Console.WriteLine(tervehdys); // Output: Hello
```

2. `Remove`-metodi:

```C#
string viesti = "Hello, World!";
string maailma = viesti.Remove(0, 7);
Console.WriteLine(maailma); // Output: World!
```

3. `Span<T>` ja `slice` C# 7.2:stä eteenpäin:

```C#
string teksti = "Hello, World!";
ReadOnlySpan<char> span = teksti.AsSpan();
ReadOnlySpan<char> hello = span.Slice(0, 5);
Console.WriteLine(hello.ToString()); // Output: Hello
```

## Deep Dive (Sukellus syvyyksiin):
Substringien käsittely on vanha konsepti, juontuu ajoista ennen C#. Alkukantaisemmilla kielillä substringien käsittely on saattanut vaatia huomattavasti enemmän työtä. C#:ssa `Substring` on ollut alusta asti, mutta uudet ominaisuudet kuten `Span<T>` tuovat parannuksia suorituskykyyn erityisesti suurten tekstien käsittelyssä. Lisäksi `Span<T>` minimoi muistinkäyttöä, sillä sen avulla voidaan viitata tekstijonon osiin muistinkopiointien sijaan.

Stringien immutaabeli luonne C#:ssa tarkoittaa, että kaikki muokkaukset luovat uusia merkkijonoja. Tämä voi olla suorituskykyongelma intensiivisessä stringien käsittelyssä. Tässä `StringBuilder` voi tulla avuksi, koska se sallii muokkaukset ilman uusia instansseja.

## See Also (Katso myös):
- MSDN-dokumentaatio `Substring`-metodista: https://docs.microsoft.com/en-us/dotnet/api/system.string.substring
- MSDN-dokumentaatio `Span<T>`-tyypistä: https://docs.microsoft.com/en-us/dotnet/api/system.span-1
- Microsoftin ohjeartikkeli `StringBuilder`:stä: https://docs.microsoft.com/en-us/dotnet/standard/base-types/stringbuilder
