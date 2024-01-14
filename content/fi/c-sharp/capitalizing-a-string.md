---
title:                "C#: Merkkijonon iso kirjoittaminen"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi Capitalize?

Capitalizen käyttäminen on tärkeää, kun haluat muuttaa kirjainkoon merkkijonossa. Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluat varmistaa, että tietokannan tallentamat tiedot ovat yhtenäisessä muodossa tai kun haluat käyttäjän syöttämää tekstiä näyttämään siistimmältä ja helpommin luettavalta. Se on myös hyödyllistä, kun työskentelet käyttäjän kanssa ja haluat välttää mahdolliset virheet tapahtumasta, kuten nimien väärin kirjoittamisesta.

## Kuinka käyttää Capitalizea?

Capitalizen käyttäminen on helppoa C#:ssa. Sinun tarvitsee vain käyttää String-luokan Capitalize-metodia ja antaa sille haluamasi merkkijono parametrina. Alla on esimerkki siitä, kuinka Capitalizea käytetään:

```C#
string s = "tämä on esimerkki";
string capitalized = s.Capitalize();
Console.WriteLine(capitalized);
```

Tämä tulostaisi "Tämä On Esimerkki" konsoliin. Huomaa, että alkuperäistä merkkijonoa ei muutettu, vaan Capitalize-metodi luo uuden merkkijonon, josta kaikki sanat on muutettu aloittamaan isolla kirjaimella.

## Syvällinen selitys Capitalizen toiminnasta

Capitalizen toiminta perustuu siihen, että se käy läpi merkkijonon sanat, ja muuttaa jokaisen sanan ensimmäisen kirjaimen isoksi. Lisäksi se muuttaa kaikki muut kirjaimet pieniksi, joten kaikki sanat ovat yhtenäisessä muodossa. Tämä tehdään käyttämällä Char-tietotyyppiä, joka edustaa yhtä merkkiä ja sen sisältämää Unicode-tietokantaa.

## Katso myös

- [String-luokan dokumentaatio](https://docs.microsoft.com/fi-fi/dotnet/api/system.string?view=net-5.0)
- [Char-tietotyypin dokumentaatio](https://docs.microsoft.com/fi-fi/dotnet/api/system.char?view=net-5.0)
- [C# perusteet - kokoelmien käsittely](https://www.tutorialsteacher.com/csharp/csharp-string)