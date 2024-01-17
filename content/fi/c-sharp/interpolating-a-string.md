---
title:                "Merkkijonon interpolointi"
html_title:           "C#: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Merkkijonon interpolointi tarkoittaa muuttujien lisäämistä merkkijonoon sen sijaan, että muuttujien arvot yhdistettäisiin merkkijonon kanssa erikseen. Tämä helpottaa ja nopeuttaa koodin kirjoittamista ja ylläpitämistä.

## Miten tehdä:
Esimerkiksi, jos haluat lisätä kaksi muuttujaa, "nimi" ja "ikä", merkkijonoon, voit käyttää interpolointia seuraavasti:
```C#
string nimi = "Matti";
int ikä = 25;
string viesti = $"Hei, olen {nimi} ja olen {ikä} vuotta vanha.";
```
Tällöin muuttujien arvot tulevat automaattisesti mukaan merkkijonoon. Lopputuloksena saat viestin "Hei, olen Matti ja olen 25 vuotta vanha."

## Syvemmällä:
Merkkijonon interpolointi on ollut käytössä C#-kielellä vuodesta 2015 lähtien, jolloin se lisättiin C# 6.0 -versioon. Aiemmin muuttujien arvot yhdistettiin merkkijonoon erikseen esimerkiksi String.Format-metodilla.

On myös mahdollista käyttää merkkijonon interpoloinnin sijaan String.Format-metodia tai yksinkertaisesti yhdistellä muuttujien arvoja "+"-operaattorilla, mutta interpolointi on usein selkeämpi ja käytännöllisempi vaihtoehto.

## Katso myös:
- [C# merkkijonon interpolointi (docs.microsoft.com)](https://docs.microsoft.com/fi-fi/dotnet/csharp/language-reference/tokens/interpolated)
- [C# String.Format-metodi (docs.microsoft.com)](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.format)
- [C# String.Concat-metodi (docs.microsoft.com)](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.concat)