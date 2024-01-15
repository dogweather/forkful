---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "C#: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Kaikki C#-ohjelmoijat kohtaavat ajoittain tarpeen etsiä ja korvata tekstiä omassa koodissaan. Tämä artikkeli auttaa sinua ymmärtämään, miten tämä tehdään kätevästi ja tehokkaasti.

## Kuinka tehdä

Usein haluamme etsiä ja korvata tiettyjä sanoja tai merkkejä tekstistä, kuten muuttaa tietyn muuttujan nimeä tai korvata tietyt tiedostopolut toisilla. Tähän tarkoitukseen C# tarjoaa hyödyllisiä toimintoja. Katso alla olevia esimerkkejä ja niiden tulos, jotka auttavat sinua ymmärtämään, miten etsimistä ja korvaamista voidaan soveltaa omassa koodissasi.

```C#
// Etsi ja korvaa sana hello-sanalla hi
string teksti = "Tervetuloa maailma! Tässä on hello!";
string uusiTeksti = teksti.Replace("hello", "hi");
Console.WriteLine(uusiTeksti); // Tulostaa: "Tervetuloa maailma! Tässä on hi!"

// Etsi ja korvaa tietty merkkijono toisella merkkijonolla
string lause = "Tämä on ensimmäinen lause. Ja tämä on toinen lause.";
lause = lause.Replace("ensimmäinen", "kolmas").Replace("toinen", "neljäs")
Console.WriteLine(lause); // Tulostaa: "Tämä on kolmas lause. Ja tämä on neljäs lause."
```

Etsimistä ja korvaamista voidaan soveltaa myös monimutkaisempiin tekstin hallintatehtäviin, kuten tietokantayhteyksiin tai tiedostojen käsittelyyn. Alla olevassa esimerkissä etsitään ja korvataan tietyn tiedoston polkua uudella polulla.

```C#
// Etsi ja korvaa tiedoston polku
string vanhaPolku = @"C:\Kansio1\Tiedosto.txt";
string uusiPolku = vanhaPolku.Replace("Kansio1", "Kansio2");
Console.WriteLine(uusiPolku); // Tulostaa: "C:\Kansio2\Tiedosto.txt"
```

## Syvempää tutustumista

Etsiminen ja korvaaminen voidaan suorittaa myös säännöllisiin lausekkeisiin perustuen, mikä mahdollistaa monipuolisemman ja tarkemman tulosten hakemisen. Voit myös hyödyntää erilaisia ​​valmiita toimintoja, kuten IndexOf ja Substring, jotka ovat tehokkaita etsimistä ja korvaamista tehtäessä.

See Also

- [C# dokumentaatio - String.Replace-metodi](https://docs.microsoft.com/fi-fi/dotnet/api/system.string.replace?view=net-5.0)
- [C# dokumentaatio - Regular expressions](https://docs.microsoft.com/fi-fi/dotnet/standard/base-types/regular-expression-language-quick-reference)