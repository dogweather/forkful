---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tietojen yhdistäminen, tai englanniksi string concatenation, tarkoittaa merkkijonojen liittämistä yhteen. Se on hyödyllinen ohjelmistokehityksen tekniikka esimerkiksi muotoilemaan viestejä tai luomaan dynaamisia ohjelmakoodin osia.

## Kuinka:

Tässä on esimerkki siitä, kuinka yhdistää merkkijonoja C-ohjelmoinnissa käyttäen `strcat()`-funktiota.

```C
#include <stdio.h>
#include <string.h>

int main() {
   char merkkijono1[50] = "Tervetuloa ";
   char merkkijono2[] = "Suomeen!";
   strcat(merkkijono1, merkkijono2);

   printf("%s\n", merkkijono1);
   return 0;
}
```

Se tulostaa:
```
Tervetuloa Suomeen!
```

## Syvällinen Sukellus

Historiallinen tausta: `strcat()`-funktio on ollut olemassa vuosikymmenien ajan, ja se on osa C-kirjaston standardia.

Vaihtoehdot: `strcat()` on hyödyllinen, mutta sen kanssa on oltava varovainen, koska se ei tarkista ylivuotoja. Saatat haluta käyttää `strncat()`, jonka avulla voit määrittää kopioitavien merkkien enimmäismäärän, jotta vältetään ylivuotot.

Yksityiskohtainen toteutus: `strcat()` löytää ensin `merkkijono1`:n pään lisäämällä sen pituuden alkuperäiseen osoitteeseen. Sitten se kopioi `merkkijono2`:n merkit `merkkijono1`:n päähän merkki kerrallaan, kunnes se saavuttaa lopun.

## Muita Lähteitä

- C Standard Library: strcat (https://www.cplusplus.com/reference/cstring/strcat/)
- Secure Coding in C and C++: String Problems (https://www.cert.org/secure-coding/research/secure-coding-in-c-and-c-string-problems.cfm)
- GNU Libc Manual: Concatenating Strings (https://www.gnu.org/software/libc/manual/html_node/Concatenating-Strings.html)