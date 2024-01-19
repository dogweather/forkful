---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstin etsiminen ja korvaaminen on toiminto, joka mahdollistaa merkkijonon tai merkkijonojen etsimisen tekstistä ja niiden korvaamisen toisella merkkijonolla. Ohjelmoijat käyttävät tätä toimintoa tekstin muokkaamiseen, datan siivoamiseen tai tiedon keräämiseen.

## Kuinka se toimii:

```C#
// C#-koodi
string alkuperainenTeksti = "Hei, olen koodari.";
string korvattavaTeksti = "koodari";
string uusiTeksti = "ohjelmoija";

// Tekstin korvaaminen
string korvattuTeksti = alkuperainenTeksti.Replace(korvattavaTeksti, uusiTeksti);

Console.WriteLine(korvattuTeksti);
```

Tästä tulee seuraava tuloste:

```
Hei, olen ohjelmoija.
```

## Syventävä tieto:

Tekstin hakeminen ja korvaaminen on ollut tärkeä osa ohjelmointia sen varhaisista päivistä lähtien. Tämä johtuu datan dynaamisesta luonteesta: asioiden on oltava muokattavissa ja päivitettävissä. C#:ssa on useita tapoja tehdä tämä, kuten `String.Replace`, `Regex.Replace` tai edistyneemmissä tapauksissa custom-algoritmeja. Näiden valinta riippuu sovelluksen tarpeista.

## Katso myös:

1. Microsoftin C#-dokumentaatio `String.Replace` -metodista: [Linkki](https://docs.microsoft.com/en-us/dotnet/api/system.string.replace?view=netcore-3.1)
2. Stack Overflow: "Miten korvata merkkijono toisella C#":ssa: [Linkki](https://stackoverflow.com/questions/1466000/how-to-replace-a-string-in-c)
3. C# Corner: "Tekstin korvaaminen C#":ssa: [Linkki](https://www.c-sharpcorner.com/article/string-replace-method-in-chash/)