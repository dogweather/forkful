---
title:    "C#: Poistetaan kuvion mukaiset merkit"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi poistaa merkkejä, jotka vastaavat tiettyä kaavaa? Yksi syy voisi olla tietojen puhdistaminen tai muotoilu ennen niiden tallentamista tai käyttämistä. Tämä on erityisen hyödyllistä, kun käsitellään suuria määriä tekstidataa ja halutaan poistaa tiettyjä merkkejä tai sanoja, jotka häiritsevät analysointia.

## Miten

Voimme käyttää C # -kielen Replace-metodia poistamaan merkkejä, jotka täsmäävät tiettyyn kaavaan. Tämä metodi korvaa kaikki vastaavat merkit tyhjällä merkinnällä, jolloin ne poistetaan kokonaan. Katso esimerkki alla olevasta koodista ja sen tuottamasta tulosteesta.

```C#
// Alkuperäinen merkkijono
string teksti = "Tervetuloa harrastamaan ohjelmointia!";

// Korvataan välilyönnit tyhjällä merkkijonolla
string puhdistettuTeksti = teksti.Replace(" ", "");

// Tulostaa: Tervetuloaharrastamaanohjelmointia!
Console.WriteLine(puhdistettuTeksti);
```

Huomaa, että Replace-metodi ottaa parametreinaan haettavan kaavan ja sen korvaavan merkkijonon. Tämä antaa meille joustavuutta tietojen puhdistamisessa, sillä voimme korvata poistettavat merkit haluamallamme sisällöllä.

## Syväsukellus

Replace-metodi käyttää säännöllisiä lausekkeita etsimään vastaavia merkkejä. Tämä tarkoittaa, että voimme käyttää monimutkaisempia kaavoja, jotka vastaavat tiettyjä merkkijonon osia. Esimerkiksi voimme poistaa kaikki isot kirjaimet merkkijonosta käyttämällä seuraavaa kaavaa: "[A-Z]". Tämä korvaa kaikki isot kirjaimet tyhjällä merkkijonolla, jolloin ne poistetaan kokonaan.

Tämän lisäksi Replace-metodi on hyödyllinen myös monissa muissa tiedonkäsittelytehtävissä, kuten tietojen muotoilussa tai korvaamisessa. Kannattaa tutustua ohjelmointikielen dokumentaatioon ja kokeilla erilaisia säännöllisiä lausekkeita saadaksesi täyden hyödyn irti Replace-metodista.

## Katso myös

- Tietoja Replace-metodin säännöllisistä lausekkeista: https://docs.microsoft.com/en-us/dotnet/standard/base-types/regular-expression-language-quick-reference
- Lisätietoja merkkijonojen käsittelystä C#:ssa: https://docs.microsoft.com/en-us/dotnet/csharp/how-to/manipulate-strings
- Esimerkkejä Replace-metodin käytöstä: https://www.c-sharpcorner.com/blogs/stringreplace-characters-in-c-sharp1