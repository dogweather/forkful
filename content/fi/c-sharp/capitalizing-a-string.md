---
title:    "C#: Merkkijonon suurennuskirjaimet"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon ensimmäisen kirjaimen isoksi? Yleensä tällaista tarvitaan, kun halutaan muotoilla käyttäjän syöttämää dataa, esimerkiksi lomakkeiden kenttiä tai hakujen tuloksia.

## Kuinka tehdä

```C#
// Alustetaan muuttuja merkkijonolle
string s = "tämä on esimerkki lauseesta.";

// Muutetaan ensimmäinen kirjain isoksi
s = char.ToUpper(s[0]) + s.Substring(1);

// Tulostetaan muutettu merkkijono
Console.WriteLine(s); // "Tämä on esimerkki lauseesta."
```

## Syvällinen sukellus

Yllä oleva koodi ei ole ainoa tapa muuttaa merkkijonon ensimmäinen kirjain isoksi, vaan on olemassa useita eri tapoja. Esimerkiksi voidaan käyttää .NET Frameworkin `TextInfo`-luokkaa, joka tarjoaa `ToTitleCase()`-metodin merkkijonon muuttamiseen. Lisäksi kannattaa ottaa huomioon kulttuurien ja kielen eroavaisuudet, sillä esimerkiksi saksassa sana "straße" muutetaan isoksi niin, että ensimmäinen kirjain on "S" mutta toinen kirjain on "s".

## Katso myös

- [C#-merkkijonot (Microsoft)](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/)
- [TextInfo-luokka (Microsoft)](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.textinfo?view=net-5.0)