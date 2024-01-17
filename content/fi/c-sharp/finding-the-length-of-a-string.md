---
title:                "Merkkijonon pituuden etsiminen"
html_title:           "C#: Merkkijonon pituuden etsiminen"
simple_title:         "Merkkijonon pituuden etsiminen"
programming_language: "C#"
category:             "C#"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c-sharp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Stringin pituuden löytäminen tarkoittaa merkkijonon sisältämien merkkien määrän laskemista. Ohjelmoijat tekevät tämän löytääkseen tarkkaa tietoa tietokoneen käsittämästä datasta.

## Miten:
Käytä seuraavaa yksinkertaista koodinpätkää löytääksesi stringin pituuden:
```
string s = "Tämä on esimerkki";
int pituus = s.Length;
Console.WriteLine(pituus);
```

Tämä koodi antaa tulosteeksi 17, sillä stringissä on 17 merkkiä, mukaan lukien välilyönnit.

## Syväsukellus:
Stringin pituuden löytäminen ei ole uusi ratkaisu ohjelmoijille. Jo alkuaikoina, jolloin ohjelmointikieliä kehitettiin, oli tarpeellista laskea merkkien määrää eri tarkoituksiin. Nykyään on olemassa myös muita tapoja löytää stringin pituus, kuten käyttämällä LINQ (Language Integrated Query) -ominaisuutta.

## Katso myös:
LINQ:in käyttö Stringin pituuden löytämiseksi: https://docs.microsoft.com/en-us/dotnet/api/system.linq.enumerable.count?view=net-5.0