---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Muutetaan Merkkijonot Pieniksi Kirjaimiksi C #: ssa

## Mikä & Miksi?
Merkkijonon muuttaminen pienaakkosiksi tarkoittaa, että jokainen merkkijonoa muodostava iso kirjain muutetaan pieneksi kirjaimeksi. Tämä on hyödyllistä esimerkiksi tehtäessä tietojen vertailua, hakua tai lajittelua, koska isot ja pienet kirjaimet tulkitaan yleensä eri merkeiksi.

## Kuinka:
Käytä sisäänrakennettua `ToLower`-metodia merkkijonon muuttamiseen pienaakkosiksi C # -koodissa.

```C#
string source = "Moi Maailma";
string result = source.ToLower();
Console.WriteLine(result);
```
Tämän koodinpätkän tuloste on: `moi maailma`.

## Syvä Sukellus:
Muuttaa isot kirjaimet pieniksi C#:ssa, kehitettiin osana tekstinkäsittelyä, missä kirjainkoolla ei ollut merkitystä. `ToLower()`-metodi on osa `String`-luokkaa .NET-kehyksessä.

Vaihtoehtoisesti voit käyttää `ToLowerInvariant`-metodia, joka käyttää kulttuuririippumatonta muunnosta, toisin kuin `ToLower`, joka käyttää kulttuurikohtaista muunnosta. Tämä on tärkeää, kun käsitellään kielikohtaisia ​​sääntöjä.

Käyttää `ToLower` tai `ToLowerInvariant` syntyy uusi merkkijono, koska .NET-merkkijonot ovat muuttumattomia. Alkuperäistä merkkijonoa ei muuteta.

## Katso Myös:
- Microsoft .NET-dokumentaatio: [ToLower](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.tolower?view=net-5.0)
- Microsoft .NET-dokumentaatio: [ToLowerInvariant](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.tolowerinvariant?view=net-5.0)