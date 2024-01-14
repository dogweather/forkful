---
title:    "C#: Merkkien poistaminen vastaavan kaavan mukaan"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmoinnin projekteissa saattaa tulla tarve poistaa merkkejä, jotka vastaavat tiettyä kaavaa. Tämä voi olla tarpeen esimerkiksi tietojen käsittelyssä tai tekstianalyysissä. Tässä blogikirjoituksessa käymme läpi, miten tämä onnistuu C# -ohjelmointikielellä.

## Miten

Poistaminen merkkejä, jotka vastaavat tiettyä kaavaa, on mahdollista C# -ohjelmoinnissa käyttämällä `Regex.Replace()` -metodia. Tämä metodi ottaa kolme parametria: ensimmäisenä merkkijonon, jonka haluamme käsitellä, toisena lausekkeen, joka kuvaa poistettavien merkkien kaavaa ja kolmantena korvaavan merkkijonon.

Alla on lyhyt esimerkki, jossa käytämme `Regex.Replace()` -metodia poistaaksemme kaikki numerot annetusta merkkijonosta:

```C#
string text = "123 abc 456";
string pattern = "[0-9]";
string replacement = "";
string result = Regex.Replace(text, pattern, replacement);
Console.WriteLine(result);
```

Tämä tulostaa " abc ". Lausekkeessa "[0-9]" määritellään, että haluamme poistaa kaikki numerot. Tämän jälkeen korvaamme ne tyhjällä merkkijonolla, jolloin saamme halutun lopputuloksen.

## Syvällinen sukellus

`Regex.Replace()` -metodi käyttää säännöllisiä lausekkeita (regular expressions) määrittelemään kaavan, joka kuvaa poistettavia merkkejä. Säännöllisillä lausekkeilla on oma syntaksinsa, joten suosittelemme lukemaan lisätietoja niistä ennen kuin aloitat niiden käytön C# -ohjelmoinnissa.

Lisäksi `Regex.Replace()` -metodi tarjoaa myös muutamia muita parametreja, jotka mahdollistavat esimerkiksi ison- tai pienikirjainten huomioimisen tai vain tietyn määrän merkkien poistamisen.

## Katso myös

- [Microsoftin ohjeet säännöllisille lausekkeille](https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference)
- [C# Regex-luokka](https://docs.microsoft.com/en-us/dotnet/api/system.text.regularexpressions.regex?view=netframework-4.8)
- [Säännöllisten lausekkeiden harjoitusalue](https://regex101.com/)