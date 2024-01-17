---
title:                "Merkkijonon yhdistäminen"
html_title:           "C#: Merkkijonon yhdistäminen"
simple_title:         "Merkkijonon yhdistäminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Miksi muodostaa yhdistettyjä merkkijonoja ohjelmoidessa ja mitä se tarkoittaa? Yhdistetyt merkkijonot ovat yksinkertaisesti kahden tai useamman merkkijonon yhdistämistä yhdeksi merkkijonoksi. Tätä voi tarvita esimerkiksi silloin, kun halutaan muodostaa uusi tekstirivi tai lisätä muuttujan arvo toisen merkkijonon perään.

## Miten:
Esimerkki muodostaen yhdistettyjä merkkijonoja ```C# Console.WriteLine("Tervetuloa " + nimi + "!");```
Esimerkki tulostaa "Tervetuloa Kalle!" mikäli muuttujaan "nimi" on tallennettu arvo "Kalle".

## Syvällinen sukellus:
Historiallista taustaa: Merkkijonojen yhdistäminen on ollut osa monia ohjelmointikieliä jo pitkään, mutta C#:ssa tämä tehtiin aluksi käyttämällä "+" operaattoria. Uudemmissa versioissa on myös mahdollista käyttää string interpolation koodin luettavuuden ja suorituskyvyn parantamiseksi.

Vaihtoehtoja: Merkkijonojen yhdistämiseen on myös muita vaihtoehtoja, kuten käyttää StringBuilder-luokkaa, joka suorituskyvyltään voi olla parempi erityisesti suurten tekstien käsittelyssä.

Toteutus: C# käyttää merkkijonojen yhdistämiseen takanaan StringBuilder-luokkaa, joka muodostaa uuden merkkijonon aina kun merkkijonoja yhdistetään. Tämä voi aiheuttaa tehokkuuden menetystä, jos merkkijonoja yhdistetään suuressa määrin.

## Katso myös:
Täältä löydät lisää tietoa C#:n merkkijonojen yhdistämisestä: https://docs.microsoft.com/fi-fi/dotnet/csharp/language-reference/operators/string-comparison