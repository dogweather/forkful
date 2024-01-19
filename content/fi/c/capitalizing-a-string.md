---
title:                "Merkkijonon pääkirjainten käyttö"
html_title:           "C: Merkkijonon pääkirjainten käyttö"
simple_title:         "Merkkijonon pääkirjainten käyttö"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Merkkijonon pääomittaminen tarkoittaa kaikkien merkkijonon kirjainten muuttamista suuriksi kirjaimiksi. Se auttaa tekemään tekstistä helposti luettavampaa ja erottuvampaa.

## Näin teet sen:

```C
#include <ctype.h>
#include <stdio.h>

void Paaomita(char s[]) {
    for(int i = 0; s[i] != '\0'; i++) {
        s[i] = toupper(s[i]);
    }
}

int main() {
    char teksti[] = "ohjelmointi on hauskaa";
    Paaomita(teksti);
    printf("%s\n", teksti); 
    return 0;
}
```

Tämä ohjelma tulostaa: `OHJELMOINTI ON HAUSKAA`

## Syvällä sukelluksella

Ohjelmoinnin alkupäivinä, kun järjestelmät olivat rajoitettuja, merkkijonon pääomittaminen auttoi säästämään arvokasta laskentatehoa. Nykypäivänä se on edelleen hyödyllinen, mutta syistä, jotka liittyvät enemmän käytettävyyteen ja luettavuuteen.

Vaihtoehtoisia tapoja merkkijonon pääomittamiseksi ovat esimerkiksi yksittäisten merkkien käsittely iteraattoreiden, kuten `for_each`, kanssa tai käyttämällä korkeamman tason kirjaston funktioita, kuten `boost::to_upper_copy`.

C:n standardikirjastossa `toupper` -funktio on toteutettu tavallisesti käyttämällä merkkikohtaista taulukkoa, joka sisältää vastaavat suuret kirjaimet. Tämä on tehokasta muistin ja nopeuden kannalta.

## Katso myös

- C-kirjasto: ctype.h ([Linkki](https://en.cppreference.com/w/c/string/byte/toupper))
- Boost kirjasto: to_upper_copy ([Linkki](https://www.boost.org/doc/libs/1_73_0/doc/html/string_algo/usage.html#id-2.9.5.8.5))
- ISO C-Standardi ([Linkki](https://www.iso.org/standard/74528.html))