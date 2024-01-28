---
title:                "Merkkijonosta lainausmerkkien poistaminen"
date:                  2024-01-26T03:38:18.571939-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonosta lainausmerkkien poistaminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lainausmerkkien poistaminen merkkijonosta tarkoittaa yksittäisten ('') tai kaksois ("") lainausmerkkien poistamista merkkijonon sisällöstä. Ohjelmoijat tekevät tämän siivotaakseen syötettä, valmistellakseen dataa jatkokäsittelyä varten tai välttääkseen syntaksivirheitä käsitellessään tiedostopolkuja ja komentoja kielissä, jotka käyttävät lainausmerkkejä merkkijonojen rajaamiseen.

## Miten:

Tässä on C-funktio, joka siivoaa nuo kiusalliset lainausmerkit pois merkkijonoistasi:

```c
#include <stdio.h>
#include <string.h>

void remove_quotes(char *str) {
    char *p_read = str, *p_write = str;
    while (*p_read) {
        if (*p_read != '"' && *p_read != '\'') {
            *p_write++ = *p_read;
        }
        p_read++;
    }
    *p_write = '\0';
}

int main() {
    char str[] = "He said, \"Hello, 'world'!\"";
    printf("Alkuperäinen: %s\n", str);
    remove_quotes(str);
    printf("Siivottu: %s\n", str);
    return 0;
}
```

Esimerkkituloste:

```
Alkuperäinen: He said, "Hello, 'world'!"
Siivottu: He said, Hello, world!
```

## Syväsukellus

Lainausmerkkien poistaminen merkkijonosta on ollut tehtävä ohjelmoinnin alkuaikoina, jolloin datan puhtaus oli ja on edelleen avain välttämään virheitä (kuten SQL-injektiohyökkäykset) tai varmistamaan, että merkkijonon voi turvallisesti siirtää järjestelmiin, jotka saattaisivat sekoittaa lainausmerkin ohjausmerkiksi.

Historiallisesti eri kielet käsittelevät tätä tehtävää eri tavoin - jotkut sisältävät valmiita funktioita (kuten `strip` Pythonissa), kun taas toiset, kuten C, vaativat manuaalisen toteutuksen, koska se keskittyy antamaan kehittäjille matalan tason hallinnan.

Vaihtoehtoja ovat kirjastofunktioiden, kuten `strpbrk`, käyttäminen lainausmerkkien etsimiseen tai säännöllisten lausekkeiden (kirjastoilla, kuten PCRE) käyttäminen monimutkaisempien mallien varalta, vaikkakin tämä saattaa olla liioittelua pelkkien lainausmerkkien poistamiseksi.

Yllä oleva toteutus yksinkertaisesti skannaa läpi jokaisen merkin merkkijonossa, kopioiden vain lainausmerkitöntä tekstiä kirjoitusosoittimen sijaintiin. Tämä on tehokasta, koska se tapahtuu paikan päällä tarvitsematta lisämuistia tulokselle.

## Katso Myös

- [C Standardikirjaston Funktiot](http://www.cplusplus.com/reference/clibrary/)
- [PCRE - Perl-yhteensopivat säännölliset lausekkeet](https://www.pcre.org/)
- [Pointereiden ymmärtäminen C:ssä](https://www.learn-c.org/en/Pointers)
