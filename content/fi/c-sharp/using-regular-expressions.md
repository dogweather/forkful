---
title:    "C#: Regulaaristen lausekkeiden käyttö"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännölliset lausekkeet ovat tärkeä osa C# ohjelmointia, sillä niitä käytetään tekstien käsittelyyn ja jäsentämiseen. Niiden avulla voidaan etsiä, korvata tai poistaa tiettyjä merkkijonoja halutusta tekstistä. Tämä tekee ohjelmista tehokkaampia ja helpompia ylläpitää.

## Miten käyttää säännöllisiä lausekkeita C# ohjelmoinnissa?

C# tarjoaa laajan valikoiman säännöllisiä lausekkeita käsitteleviä luokkia ja metodeja, jotka tekevät niiden käytöstä melko suoraviivaista. Seuraavassa on esimerkkejä yleisimmin käytetyistä säännöllisiin lausekkeisiin liittyvistä toiminnoista C# ohjelmoinnissa:

- Etsi tietty teksti: ```C# Regex.Match("teksti", "t"); // Palauttaa "t" ```
- Korvaa teksti toisella: ```C# Regex.Replace("Tämä on esimerkki", "esimerkki", "demo"); // Palauttaa "Tämä on demo" ```
- Tarkista onko teksti halutussa muodossa: ```C# Regex.IsMatch("12345", @"\d{5}"); // Palauttaa true ```
- Etsi useita vaihtoehtoisia merkkijonoja: ```C# Regex.Match("C# on mahtava kieli", "mahtava|upea"); // Palauttaa "mahtava" ```

Näiden lisäksi C# tarjoaa myös muita hyödyllisiä metodeja, kuten ```Regex.Split()``` ja ```Regex.Escape()```, jotka helpottavat säännöllisten lausekkeiden käyttöä ohjelmoinnissa.

## Syvempi sukellus säännöllisiin lausekkeisiin

C# säännöllisten lausekkeiden taustalla on .NET Frameworkin ```Regex``` luokka, joka sisältää kaikki tarvittavat luokat ja metodit niiden käsittelemiseen. Tämän luokan avulla voimme luoda oman luokkamme, joka käsittelee tietynlaisten säännöllisten lausekkeiden käsittelyä. Tämä antaa meidän hyödyntää niitä omassa ohjelmoinnissa entistä joustavammin.

On myös tärkeää muistaa, että säännölliset lausekkeet ovat melko tehokkaita ja nopeita, mutta ne voivat myös olla hankalia ymmärtää ja käyttää oikein. Siksi on suositeltavaa harjoitella ja tutustua huolellisesti niiden käyttöön ennen niiden käytön aloittamista.

## Katso myös

- [C# Regular Expressions Tutorial](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expressions)
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)
- [.NET Regular Expressions Reference](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)