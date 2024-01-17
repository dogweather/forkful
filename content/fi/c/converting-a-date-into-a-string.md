---
title:                "Päivämäärän muuntaminen merkkijonoksi"
html_title:           "C: Päivämäärän muuntaminen merkkijonoksi"
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Päivämäärien muuntaminen merkkijonoksi tarkoittaa päivämäärien muuntamista erottimilla erotelluksi tekstimuodoksi. Ohjelmoijat tekevät tämän esimerkiksi käyttäjän syöttämän päivämäärän tallentamiseksi tai päivämäärän tulostamiseksi käyttökelpoisessa muodossa.

## Miten se tehdään:
Esimerkkejä koodaamisesta ja esimerkkitulostuksista ```C ... ``` koodipalikoissa.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Luo rakenne aika ja aseta haluttu päivämäärä
    struct tm date = { .tm_mday = 31, .tm_mon = 12, .tm_year = 2020 - 1900 };
    
    // Tulosta päivämäärä merkkijonona
    printf("%d.%d.%d", date.tm_mday, date.tm_mon, date.tm_year + 1900);
    
    return 0;
}
```

Tulos:

```
31.12.2020
```

## Syvällinen sukellus:
1. Historiallinen konteksti:
Aiemmin ohjelmoijat joutuivat muokkaamaan päivämääriä manuaalisesti ja kovakoodaamaan ne ohjelmiin. Nykyään päivämäärien muuntaminen merkkijonoksi on helppoa ja kätevää käyttäen apuna valmiita funktioita.
2. Vaihtoehtoiset tavat:
Päivämäärän muuntaminen merkkijonoksi voidaan tehdä myös käyttämällä toisenlaisia funktioita tai kirjastoja, kuten `strftime()` tai `strptime()`.
3. Toteutusyksityiskohdat:
Päivämäärien muuntaminen merkkijonoksi voidaan toteuttaa esimerkiksi jakamalla päivämäärä parametreiksi, joista jokainen edustaa yhtä osaa (päivä, kuukausi, vuosi). Näitä parametreja hyödyntäen voidaan koostaa haluttu merkkijono.

## Katso myös:
- [C-kirjaston aikatyökalut](https://www.tutorialspoint.com/c_standard_library/c_function_struct_tm.htm)
- [strftime-funktion dokumentaatio](https://www.cplusplus.com/reference/ctime/strftime/)
- [strptime-funktion dokumentaatio](https://www.cplusplus.com/reference/ctime/strptime/)