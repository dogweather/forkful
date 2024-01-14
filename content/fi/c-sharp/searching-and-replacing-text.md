---
title:                "C#: Tekstin etsiminen ja vaihtaminen"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kuka tahansa haluaisi etsiä ja korvata tekstiä ohjelmointiprojektissaan? Yksinkertaisesti sanottuna, etsimis- ja korvaamistoiminnot voivat säästää paljon aikaa ja vaivaa. Sen sijaan, että kävisit manuaalisesti läpi tuhansia rivejä koodia korjatakseen yksittäisiä kirjoitusvirheitä, ohjelmoijat voivat käyttää etsi ja korvaa -toimintoa automaattisesti korjaamaan ja päivittämään tekstiä.

## Miten

Alla on esimerkki C#-koodista, joka näyttää, kuinka etsiä ja korvata tekstiä käyttäen Regular Expression -kirjastoa. Tämä esimerkki käy läpi tiedoston ja korvaa kaikki "tämä" -sanat painikkeeseen "se". Huomaa, että koodi on vain esimerkki, ja sitä voi muokata tarpeidesi mukaan.

```C#
using System;
using System.Text.RegularExpressions; // Sisällytetään Regular Expression kirjasto

string teksti = "Tämä on esimerkki, jossa etsitään ja korvataan sanoja.";
Console.WriteLine($"Alkuperäinen teksti: {teksti}"); // Tulostaa alkuperäisen tekstin

string korvattu = Regex.Replace(teksti, "tämä", "se"); // Käytetään Replace-metodia korvaamaan kaikki "tämä" sanat "se" sanalla
Console.WriteLine($"Korjattu teksti: {korvattu}"); // Tulostaa korjatun tekstin
```

Esimerkkikoodin tulostus:

```
Alkuperäinen teksti: Tämä on esimerkki, jossa etsitään ja korvataan sanoja.
Korjattu teksti: Se on esimerkki, jossa etsitään ja korvataan sanoja.
```

## Syväsukellus

Regular Expression -kirjasto on tehokas työkalu tekstien hakemiseen ja korvaamiseen. Se sisältää paljon erilaisia metodeja ja vaihtoehtoja, joiden avulla voit tarkentaa hakuasi ja tehdä erilaisia muutoksia tekstiin. Esimerkiksi voit määrittää, haluatko korvata vain ensimmäisen esiintymän vai kaikki esiintymät, haluatko tehdä kirjainkohtaisen vai kokonaisen sanan korvauksen jne.

On myös tärkeää huomata, että Regular Expression -kirjasto käyttää erityisiä merkintöjä (esimerkiksi sulkumerkit ja takaisinviitaukset) tarkoittamaan erityisiä hakuja ja korvauksia. Siksi on tärkeää ymmärtää nämä merkinnät ennen kuin aloitat etsimisen ja korvaamisen käyttämisen.

## Katso myös

* [Regular Expression kirjasto (C#)](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
* [C# syntaksi tutorial](https://www.codecademy.com/learn/learn-c-sharp)