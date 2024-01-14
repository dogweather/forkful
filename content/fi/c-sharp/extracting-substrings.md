---
title:                "C#: Irrottavien alimerkkijonojen hankkiminen"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien erottaminen on tärkeä osa C#-ohjelmointia, sillä se mahdollistaa tietyn osan merkkijonosta erottamisen ja tarkastelun. Tämä on erityisen hyödyllistä, kun halutaan käsitellä tilanteita, joissa tietyn merkkijonon osaa tarvitaan erillisenä.

## Kuinka

Alla on esimerkki siitä, kuinka C#-ohjelmointikielessä voidaan erottaa merkkijonon osia käyttämällä substrings-toimintoa ja tulostaa ne konsolille.

```C#
// Luodaan merkkijono
string teksti = "Tämä on esimerkki substringien erottamisesta";

// Erotaan merkkijonon osa ja tulostetaan se konsolille
string osa = teksti.Substring(16, 11);
Console.WriteLine(osa);

// Tulostaa: "substringien"
```

Tässä esimerkissä luomme ensin merkkijonon ja tallennamme sen muuttujaan nimeltä "teksti". Sitten käytämme "Substring()" -funktiota, joka ottaa kaksi parametria - aloituskohdan ja erottamisen pituuden - ja tallentaa erottamamme osan muuttujaan nimeltä "osa". Lopuksi tulostamme tämän osan konsolille.

## Syvempi sukellus

Substringien erottaminen osoittautuu erittäin hyödylliseksi, kun halutaan käsitellä isot sunkeet erilaista tietoa sisältävien merkkijonojen kanssa. C# tarjoaa myös muita vaihtoehtoja, kuten "Remove()", "Insert()" ja "Replace()", jotka voivat auttaa käsittelemään merkkijonoja ja niiden osia tehokkaasti.

On myös tärkeää huomata, että C#-ohjelmointikielen dynaamisten ominaisuuksien avulla voimme käsitellä merkkijonojen osia dynaamisesti, mikä lisää vielä enemmän joustavuutta koodiin.

## Katso myös

- [C#-ohjelmointikielen dokumentaatio merkkijonojen käsittelystä](https://docs.microsoft.com/fi-fi/dotnet/csharp/programming-guide/strings/)
- [Merkkijonojen käsittelyn perusteet C#:ssä](https://www.tutlane.com/tutorial/csharp/csharp-strings)
- [Substringien käyttöesimerkkejä C#:ssä](https://www.w3schools.com/cs/cs_ref_string.asp)