---
title:                "Kuvion mukaisten merkkien poistaminen"
html_title:           "C#: Kuvion mukaisten merkkien poistaminen"
simple_title:         "Kuvion mukaisten merkkien poistaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi & Mikä?
Miksi ohjelmoijat poistavat merkkejä, jotka vastaavat tiettyä kuvioa? Se tehdään yleensä tietyn tuloksen saavuttamiseksi, esimerkiksi tietyn merkkijonon löytämiseksi tiedostosta tai tietokannasta. Tähän voi myös liittyä tiettyjen tietojen suodattaminen tai muokkaaminen.

## Kuinka:
Seuraavassa esimerkissä näytämme, kuinka voit poistaa kaikki sitaatimerkit merkkijonosta käyttämällä Regex.Replace-metodia:

```C#
string lause = "Tämä on "testi" lause.";
string korjattuLause = Regex.Replace(lause, "\"", "");
Console.WriteLine(korjattuLause);
//Tulostaa: Tämä on testi lause.
```

## Syvällisemmin:
Tässä tapauksessa olemme käyttäneet Regular Expression -kirjastoa (Regex) poistaaksemme kaikki merkit, jotka vastaavat " ja " -kuvioita. Tähän menetelmään liittyy kuitenkin myös muita vaihtoehtoja, kuten string.Replace-metodi tai Substring-metodi.

Regex-metodilla pystytään myös suorittamaan monimutkaisempia kuvioita, kuten säännöllisiä lausekkeita, jotka mahdollistavat tarkemman haun halutuista merkkijonoista.

## Katso myös:
Voit lukea lisää Regex-kirjastosta täältä: 
[Microsoftin Regex-dokumentaatio.](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)

Lisätietoa merkkijonojen muokkaamisesta löytyy täältä: [String.Replace-metodi dokumentaatio.](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)

Voit myös harjoitella Regex-kuvion luomista ja testaamista täällä: [Regex-testeri.](https://regex101.com/)