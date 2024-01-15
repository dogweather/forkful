---
title:                "Alimerkkien erottaminen"
html_title:           "C: Alimerkkien erottaminen"
simple_title:         "Alimerkkien erottaminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringeja voidaan käyttää monissa eri tilanteissa ohjelmoinnissa, kuten merkkijonojen manipuloinnissa tai datan käsittelyssä. Niiden avulla voidaan myös helpottaa ja tehostaa tietojen etsimistä ja käsittelyä.

## Miten

Substringien poimiminen on helppoa C-ohjelmoinnissa. Käytämme tähän memcpy-funktiota, joka kopioi halutun määrän merkkejä yhdestä merkkijonosta toiseen. Esimerkiksi jos haluamme poimia merkkijonosta "Hello world!" sanan "Hello", voimme käyttää seuraavaa koodia:

```C
char str1[] = "Hello world!";
char str2[6];

memcpy(str2, str1, 5);
str2[5] = '\0';

printf("%s", str2);
```

Tämän koodin tulos olisi "Hello". Koodissa määritellään ensin alkuperäinen merkkijono ja sen jälkeen toinen merkkijono, johon haluamme poimia substrings. Käytämme memcpy-funktiota, johon annamme parametreina kopioitavan merkkijonon, kohde-merkkijonon ja halutun määrän merkkejä. Lopuksi tarvitaan vielä muistinvaraus ja lopussa tulostetaan poimittu substring.

## Syvällisempi sukellus

Substringit ovat myös osa C-kielen sisäänrakennettuja string-funktioita, kuten strcat, strcpy ja strstr. Substringien avulla voidaan helposti käsitellä merkkijonoja ja poimia tietoa halutuista kohdista. Tässä on muutama esimerkki substringien käytöstä:

- Voit poimia merkkijonosta tietyn määrän merkkejä alkaen tietystä indeksistä memcpy-funktion avulla.
- Voit etsiä tietyltä alueelta merkkijonoja strstr-funktiolla ja käyttää niitä osana erilaisia ehtoja ja operaatioita.
- Voit myös yhdistellä eri merkkijonoja strcat-funktiolla ja poimia niistä tietyt osat käyttämällä substringeja.

Kaiken kaikkiaan substringien käyttö on hyödyllistä ja monipuolista C-ohjelmoinnissa. Niiden avulla pystytään helposti käsittelemään ja muokkaamaan merkkijonoja erilaisissa tilanteissa ja tehostamaan koodin toimintaa.

## Katso myös

- Standardi string-funktioihin: https://www.tutorialspoint.com/c_standard_library/string_h.htm
- C-kielen memcpy-funktio: https://www.tutorialspoint.com/c_standard_library/c_function_memcpy.htm
- Substringien käyttö: https://www.techbeamers.com/c-string-processing-functions/
- Miksi memcpy on parempi kuin strncpy: https://stackoverflow.com/questions/16515004/why-is-memcpy-in-c-better-than-strncpy