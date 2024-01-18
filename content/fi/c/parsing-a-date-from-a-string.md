---
title:                "Päivämäärän erittely merkkijonosta"
html_title:           "C: Päivämäärän erittely merkkijonosta"
simple_title:         "Päivämäärän erittely merkkijonosta"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Päivämäärän parsiminen merkkijonosta tarkoittaa päivämäärän muuntamista merkkijonoksi. Tämä on hyödyllistä ohjelmointissa esimerkiksi käyttäjän antaman päivämäärän tarkistamiseksi tai tallentamiseksi tietokantaan.

## Kuinka:

```C
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main()
{
  char input[20];
  struct tm date;
  time_t epoch;

  printf("Anna päivämäärä (dd.mm.yyyy): ");
  scanf("%s", input);

  strptime(input, "%d.%m.%Y", &date);
  epoch = mktime(&date);
  printf("Päivämäärä muunnettuna aikaleimaksi: %ld", epoch);

  return 0;
}
```

**Tulos:**

```
Anna päivämäärä (dd.mm.yyyy): 01.01.2021
Päivämäärä muunnettuna aikaleimaksi: 1609459200
```

## Syvällisempi tarkastelu:

Päivämäärän parsiminen merkkijonosta on ollut tarpeellista jo aikojen alusta lähtien, kun tietojenkäsittely aloitettiin. Nykyään tarjolla on useita tapoja suorittaa tämä tehtävä, kuten käyttämällä kirjastoja tai itse koodaamalla. Tärkeää on huomioida, että käytetty parsimistapa vaikuttaa myös suorituskykyyn.

## Katso myös:

- [strptime - The GNU C Library](https://www.gnu.org/software/libc/manual/html_node/Date-and-Time-Parsing.html)
- [mktime - The GNU C Library](https://www.gnu.org/software/libc/manual/html_node/Time-Types.html#Time-Types)