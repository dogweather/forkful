---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Merkinjonojen interpolointi tarkoittaa muuttujien syöttämistä siihen suoraan. Se nopeuttaa ja selkiyttää koodia, koska voimme lisätä muuttuja-arvon suoraan merkkijonoon ohjelman suorituksen aikana.

## Miten tehdä:
C-kielessä käytämme sprintf()-funktiota interpoloinnin toteuttamiseksi. Tässä on esimerkki:

```C
#include<stdio.h>

int main() {
   int age = 30;
   char str[20];

   sprintf(str, "Ikäsi on %d", age);
   printf("%s\n", str);

   return 0;
}
```

Tämän koodin tulostus olisi: `Ikäsi on 30`

## Syväsukellus
C-kielessä merkkijonojen interpolointi ei ole sisäänrakennettu ominaisuus kuten jotkut muut kielet (Python, JavaScript), sen sijaan käytämme funktionaalisia ratkaisuja, kuten sprintf(). Muita vaihtoehtoja tarjoaa C Standards Library, kuten snprintf() ja asprintf(), jotka ovat samankaltaisia, mutta niillä on erilaiset käyttötapaukset ja turvallisuusominaisuudet. Koodin optimointi ja muistinhallinta on harkittava interpoloinnin toteutuksessa.

## Katso myös
1. sprintf() dokumentaatio: https://en.cppreference.com/w/c/io/fprintf
2. snprintf() dokumentaatio: https://en.cppreference.com/w/c/io/fprintf
3. asprintf() dokumentaatio: https://man7.org/linux/man-pages/man3/asprintf.3.html
4. Stringit C:ssä: https://www.learn-c.org/en/Strings