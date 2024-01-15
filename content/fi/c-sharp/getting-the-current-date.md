---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "C#: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Usein tarvitsemme nykyisen päivämäärän ja ajan tietoa sovelluksissamme. Onneksi C# tarjoaa helpon ja tehokkaan tavan saada tämä tieto ohjelmamme käyttöön. 

## Miten
Tähän löytyy useita tapoja käyttää ohjelman päivämäärä- ja aikatoimintoja. Käytämme tässä esimerkkeinä DateTime-luokkaa ja sen ominaisuuksia. 

```C#
// Luodaan uusi DateTime-olio, joka sisältää nykyisen päivämäärän ja ajan
DateTime nykyinenPvmJaAika = DateTime.Now;

// Voimme myös saada pelkän päivämäärän käyttämällä Date-ominaisuutta
DateTime nykyinenPvm = nykyinenPvmJaAika.Date;

// Voidaan esimerkiksi tulostaa nykyisen päivämäärän ja ajan konsoliin
Console.WriteLine("Nykyinen päivämäärä ja aika: " + nykyinenPvmJaAika);
Console.WriteLine("Nykyinen päivämäärä: " + nykyinenPvm);
```

**Tulostaa:**
```
Nykyinen päivämäärä ja aika: 9/3/2021 11:45:28 AM
Nykyinen päivämäärä: 9/3/2021 12:00:00 AM
```

Voimme myös halutessamme muuttaa päivämäärän ja ajan formaattia `ToString()`-metodin avulla.

```C#
// Muutetaan päivämäärän formaatti muotoon pp.kk.vvvv
string muokattuPvm = nykyinenPvm.ToString("dd.MM.yyyy");
// Tulostaa: 03.09.2021
Console.WriteLine(muokattuPvm);
```

## Syvällisempi sukellus
DateTime-luokassa on monia muita hyödyllisiä ominaisuuksia, kuten mahdollisuus hakea tietoja tietystä päivämäärästä tai verrata kahta päivämäärää keskenään. Voit tutustua tarkemmin DateTime-luokkaan Microsoftin virallisella verkkosivustolla: [DateTime-luokka (C#-ohjelmointiopas)](https://docs.microsoft.com/fi-fi/dotnet/api/system.datetime?view=net-5.0).

## Katso myös
[C#-ohjelmointiopas (Microsoft)](https://docs.microsoft.com/fi-fi/dotnet/csharp/) 

[C# DateTime - Documentation (W3Schools)](https://www.w3schools.com/cs/cs_date_time.asp) 

[C# DateTime - How to format DateTime? (Web Code Geeks)](https://www.webcodegeeks.com/c-sharp/datetime-convert-format-csharp/)