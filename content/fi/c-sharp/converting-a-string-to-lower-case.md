---
title:                "C#: Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa tarvitaan tapoja muuttaa tekstiä eri muotoihin ja yksi näistä muodoista on pienet kirjaimet. Pienen kirjaimen muotoilua käytetään esimerkiksi silloin kun vertaillaan sanoja tai halutaan saada yhtenäinen muoto tekstin käsittelyä varten. Tässä blogikirjoituksessa tutustumme siihen, kuinka muuttaa merkkijono pieniksi kirjaimiksi käyttäen C#-ohjelmointikieltä.

## Kuinka

Jos haluat muuttaa merkkijonon pieniksi kirjaimiksi C#-ohjelmassa, sinun tarvitsee käyttää String.ToLower() metodia. Alla on esimerkki koodista, jossa teksti muutetaan pieniksi kirjaimiksi ja tulostetaan konsoliin:

```C#
string teksti = "TÄMÄ ON ESIMERKKI";
string pienilläKirjaimilla = teksti.ToLower();
Console.WriteLine(pienilläKirjaimilla);
```

Tulos:
```
tämä on esimerkki
```

Kuten näemme esimerkistä, String.ToLower() metodi muuttaa kaikki isot kirjaimet pieniksi kirjaimiksi. Metodia voidaan käyttää myös yksittäisille merkeille, esimerkiksi jos haluaisimme muuttaa vain ensimmäisen kirjaimen pieneksi:

```C#
string teksti = "TÄMÄ ON UUSI MERKKIJONO";
char ensimmäinenKirjain = char.ToLower(teksti[0]);
Console.WriteLine(ensimmäinenKirjain + teksti.Substring(1));
```

Tulos:
```
tÄMÄ ON UUSI MERKKIJONO
```

## Syvällinen sukellus

Miten sitten tämä pieniksi muuttaminen tapahtuu taustalla? C#-kielen String-luokka tarjoaa String.ToLower() metodin, jolloin metodi ottaa vastaan alkuperäisen merkkijonon ja palauttaa uuden merkkijonon, jossa kaikki isot kirjaimet on muutettu pieniksi. Merkkijonon muokkaaminen tapahtuu käyttäen Unicode Standardin String.ToLowerInvariant() metodia, jolloin kaikki kirjaimet on järjestetty Unicode Standardin mukaisesti.

## Katso myös

- [C# String.ToLower() metodi](https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower)
- [Unicode Standard](https://unicode.org/standard/standard.html)