---
title:                "Päivämäärän jäsentäminen merkkijonosta"
html_title:           "Bash: Päivämäärän jäsentäminen merkkijonosta"
simple_title:         "Päivämäärän jäsentäminen merkkijonosta"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Päivämäärän jäsennys merkkijonosta C-ohjelmointikielessä tarkoittaa päivämäärän lukemista merkkij perusta. Ohjelmoijat tekevät näin, kun heidän pitää käsitellä päivämääriä, jotka on syötetty ohjelmaan tekstimuodossa.

## Kuinka toimitaan:

Parse päivämäärä merkkijonosta käyttäen `strptime` funktiota:

```C
#include <time.h>
#include <stdio.h>

int main(void) {
    struct tm tm;
    char buf[256];

    strptime("2021-09-05", "%Y-%m-%d", &tm);
    strftime(buf, sizeof(buf), "%d-%m-%Y", &tm);

    puts(buf);

    return 0;
}
```

Tämä programmi tulostaa päivämäärän uudessa formaatissa: `05-09-2021`.

## Syvällinen tarkastelu

Historiallinen konteksti: `strptime` funktio on osa POSIX-standardia, joka on ollut olemassa jo 1980-luvulta lähtien.

Vaihtoehtoja: Voit myös käyttää kolmannen osapuolen kirjastoja, kuten [Boost.Date_Time](https://www.boost.org/doc/libs/1_69_0/doc/html/date_time.html).

Jäsennyksen yksityiskohdat: `strptime` ottaa merkkijonon "2021-09-05", jäsentelee sen siihen sisältyvien - erikseen määriteltyjen - mallien mukaan ja tallentaa tiedot `tm`-rakenteeseen.

## Katso myös

Lue lisää päivämäärien jäsennyksestä merkkijonoista ja `strptime` funktion käytöstä näistä lähteistä:
- [C Library - <time.h> (tutorialspoint.com)](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Date and time functions (cplusplus.com)](http://www.cplusplus.com/reference/ctime/)
- [C Date and Time (programiz.com)](https://www.programiz.com/c-programming/library-function/time.h/strptime)