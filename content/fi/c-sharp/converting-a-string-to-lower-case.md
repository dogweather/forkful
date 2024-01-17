---
title:                "Merkkijonon muuntaminen pienaakkosiksi"
html_title:           "C#: Merkkijonon muuntaminen pienaakkosiksi"
simple_title:         "Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

##
Mitä ja miksi?

Merkkijonon muuntaminen pieniksi kirjaimiksi on yleinen toimenpide, jota ohjelmoijat tekevät käsitellessään merkkijonoja C#-koodissaan. Pieniksi muuttaminen tarkoittaa kaikkien merkkijonon kirjainten muuttamista pieniksi kirjaimiksi. Tämä on tärkeää esimerkiksi silloin, kun emme halua tehdä eroa merkkijonossa olevan kirjaimen koolla.

Miten tehdään:

Tämä voidaan tehdä helposti käyttämällä C#: n toUpper() -menetelmää. Tämä toiminto muuntaa kaikki merkkijonon kirjaimet pieniksi kirjaimiksi. Alla on yksinkertainen esimerkki, miten tämä voidaan tehdä:

```C#
string s = "Tämä On Esimerkki!";
string lower = s.ToLower();

Console.WriteLine(lower); // tulostaa: tämä on esimerkki!
```

Mikäli haluamme vain tarkistaa yksittäisen merkin koon, voimme käyttää Char.ToLower()-metodia. Tämä metodi ottaa parametrina vastaan yksittäisen merkin ja muuttaa sen pienenä. Alla on esimerkki tästä:

```C#
char c = 'A';
c = Char.ToLower(c);

Console.WriteLine(c); // tulostaa: a
```

## Syvemmälle

Historiallisessa kontekstissa, tänä päivänä, pieniksi muuttaminen tapahtuu paljon pienellä tasolla, jota emme edes huomaa. Monet ohjelmistot käyttävät pieniksi muunninta automaattisesti käsitellessään käyttäjän syöttöjä, jotta voitaisiin varmistaa yhtenäinen muotoilu.

On myös olemassa muita tapoja muuntaa merkkijono pieniksi kirjaimiksi, joista yleisin on käyttää String.ToLower() -menetelmää. Tämä on käytännöllinen silloin, kun haluamme tosiasiallisesti muuttaa alkuperäistä merkkijonoa, sen sijaan että haluamme vain luoda uuden merkkijonon.

## Katso myös

Lisätietoja merkkijonon muuntamisesta löytyy C#:n virallisesta dokumentaatiosta osoitteessa https://docs.microsoft.com/en-us/dotnet/api/system.string.tolower?view=netcore-3.1. Siellä löydät myös muita hyödyllisiä merkkijonon manipuloinnin menetelmiä.