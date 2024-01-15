---
title:                "Alimerkkijonojen erottelu"
html_title:           "C#: Alimerkkijonojen erottelu"
simple_title:         "Alimerkkijonojen erottelu"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on tärkeä taito ohjelmoinnissa, joka auttaa helpottamaan monimutkaisten merkkijonojen käsittelyä. Yleinen syy tähän on tiettyjen tietojen etsiminen tai manipulointi halutusta merkkijonosta, esimerkiksi tietyn sanan tai numeron etsiminen.

## Näin teet sen

Voit käyttää C# -ohjelmointikieltä helposti ja tehokkaasti erottamaan substringeja käytännöllisillä esimerkeillä ja kooditulosteilla.

```C#
// Luo merkkijono
string teksti = "Tämä on esimerkkiteksti.";

// Tulostaa tekstisi pituuden
Console.WriteLine($"Tekstiä on yhteensä {teksti.Length} merkkiä.");

// Tulostaa ensimmäiset 10 merkkiä
string partial = teksti.Substring(0, 10);
Console.WriteLine($"Substring: {partial}");

// Tulostaa viimeisen 10 merkkiä
partial = teksti.Substring(teksti.Length - 10);
Console.WriteLine($"Viimeiset 10 merkkiä: {partial}");

// Tulostaa merkkien lukumäärän sanat:
int sanat = teksti.Split(' ').Length;
Console.WriteLine($"Tekstissä on yhteensä {sanat} sanaa.");

// Etsii ja tulostaa tietyn sanan sijainnin tekstissä
int sijainti = teksti.IndexOf("esimerkki");
Console.WriteLine($"Ensimmäinen esiintyminen sijaitsee indeksissä {sijainti}.");

// Korvaa sanan ja tulostaa muokatun tekstin
teksti = teksti.Replace("esimerkki", "harjoitusesimerkki");
Console.WriteLine($"Muokattu teksti: {teksti}");
```

Output:

```
Tekstiä on yhteensä 23 merkkiä.
Substring: Tämä on e
Viimeiset 10 merkkiä: erkkiteksti.
Tekstissä on yhteensä 4 sanaa.
Ensimmäinen esiintyminen sijaitsee indeksissä 8.
Muokattu teksti: Tämä on harjoitusesimerkkiteksti.
```

## Syvempi sukellus

C# tarjoaa monia käteviä metodeja substringien erottelemiseen, kuten `Substring()`, `Split()` ja `IndexOf()`. Näiden avulla voit helposti etsiä ja manipuloida haluamiasi tekstin paloja. On myös tärkeää huomioida, että merkkijonot ovat C# -kielellä muuttumattomia, joten `Substring()` ja `Replace()` -metodit luovat aina uuden merkkijonon sen sijaan, että muokkaisivat alkuperäistä.

## Katso myös

- [Microsoftin dokumentaatio substringien erottelusta C# -kielellä](https://docs.microsoft.com/en-us/dotnet/csharp/how-to/substring)
- [Kaikki C# -kielen string-metodit](https://docs.microsoft.com/en-us/dotnet/api/system.string?view=netcore-3.1)
- [C# String-tietotyyppi](https://www.tutlane.com/tutorial/csharp/csharp-string-data-type)