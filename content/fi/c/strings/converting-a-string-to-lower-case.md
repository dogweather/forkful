---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:37.060230-07:00
description: "Merkkijonon muuntaminen pieniksi kirjaimiksi C-kieless\xE4 sis\xE4lt\xE4\
  \xE4 sen, ett\xE4 kaikki annetun merkkijonon isot kirjaimet muunnetaan vastaaviksi\
  \ pieniksi\u2026"
lastmod: '2024-03-13T22:44:57.024975-06:00'
model: gpt-4-0125-preview
summary: "Merkkijonon muuntaminen pieniksi kirjaimiksi C-kieless\xE4 sis\xE4lt\xE4\
  \xE4 sen, ett\xE4 kaikki annetun merkkijonon isot kirjaimet muunnetaan vastaaviksi\
  \ pieniksi\u2026"
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
---

{{< edit_this_page >}}

## Mikä ja miksi?

Merkkijonon muuntaminen pieniksi kirjaimiksi C-kielessä sisältää sen, että kaikki annetun merkkijonon isot kirjaimet muunnetaan vastaaviksi pieniksi kirjaimiksi. Ohjelmoijat suorittavat usein tämän toimenpiteen, jotta tekstisyöte standardoitaisiin vertailua, hakuoperaatioita varten, tai yksinkertaisesti ulostulon esteettisen yhtenäisyyden vuoksi.

## Kuinka:

C:ssä ei ole sisäänrakennettua funktiota merkkijonon muuntamiseksi suoraan pieniksi kirjaimiksi, toisin kuin jotkut korkean tason kielet. Tämä prosessi voidaan kuitenkin helposti toteuttaa käyttämällä C-standardikirjaston funktioita. Alla on askel askeleelta opas ja esimerkki siitä, miten merkkijono muunnetaan pieniksi kirjaimiksi.

```c
#include <stdio.h>
#include <ctype.h>

void toLowerCase(char *str) {
    while (*str) {
        *str = tolower(*str);
        str++;
    }
}

int main() {
    char text[] = "Hello, World!";
    printf("Alkuperäinen: %s\n", text);

    toLowerCase(text);
    printf("Pienikirjaiminen: %s\n", text);

    return 0;
}
```

**Esimerkkituloste:**

```
Alkuperäinen: Hello, World!
Pienikirjaiminen: hello, world!
```

Esimerkissä `toLowerCase` funktio käy läpi jokaisen syötemerkkijonon merkin, muuntaen sen pienikirjaimiseksi vastineeksi käyttäen `tolower` funktiota `ctype.h`:sta. Muutos tehdään paikan päällä, muokaten alkuperäistä merkkijonoa.

## Syvemmälle

Esimerkissä yllä käytetty `tolower` funktio on osa C-standardikirjastoa, erityisesti `ctype.h` otsikkotiedostossa. Se toimii nykyisen lokaalin perusteella, mutta standardille "C" lokaalille se käsittelee ASCII-merkistöä, jossa 'A':sta 'Z':aan muunnetaan 'a':ksi 'z':aan.

Historiallisesti merkistöjen koodaamisen ja kirjainkoon muuntamisen käsittely C:ssä oli tiiviisti sidoksissa ASCII-merkistöön, rajoittaen sen hyödyllisyyttä kansainvälisissä tai lokaaleissa sovelluksissa, joissa tavalliset merkit ASCII-merkistön ulkopuolelta ovat yleisiä. Modernit ohjelmointikielet saattavat tarjota sisäänrakennettuja merkkijonometodeja suorittamaan kirjainkoon muunnoksen ottaen huomioon lokaalin ja Unicode-merkit, joita C:ssä ei ole natiivisti.

Skenaarioissa, jotka vaativat laajaa tekstinkäsittelyä, erityisesti ei-ASCII merkkien kanssa, ohjelmoijat saattavat harkita kirjastojen käyttämistä, jotka tarjoavat parempaa kansainvälistämisen tukea, kuten ICU (International Components for Unicode). Kuitenkin useimmissa sovelluksissa, jotka käsittelevät ASCII-tekstiä, esitelty lähestymistapa on tehokas ja suoraviivainen. Se korostaa C:n taipumusta antaa ohjelmoijille hallinta datan manipuloinnissa, vaikkakin se vaatii hieman enemmän vaivannäköä verrattuna korkean tason kieliin.
