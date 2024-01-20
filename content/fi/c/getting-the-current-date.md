---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:12:54.342783-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Kellonajan ja päivämäärän haku tarkoittaa nykyhetken ajastamptimperkin selvittämistä. Ohjelmoijat tarvitsevat tätä muun muassa logien aikaansaamiseen, aikaleimojen antamiseen tiedostoille ja käyttäjien toimien aikajanan seuraamiseen.

## How to: (Miten tehdään:)
```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t nykyhetki;
    struct tm *aikaRakenne;

    time(&nykyhetki);  // Nykyinen aika epoch-lukuna
    aikaRakenne = localtime(&nykyhetki);  // Muunna ihmismuotoon

    // Tulosta päivämäärä ja aika
    printf("Tänään on: %02d.%02d.%d klo %02d:%02d\n",
           aikaRakenne->tm_mday, aikaRakenne->tm_mon + 1, aikaRakenne->tm_year + 1900,
           aikaRakenne->tm_hour, aikaRakenne->tm_min);

    return 0;
}

```

Esimerkki tulostus:
```
Tänään on: 02.04.2023 klo 15:30
```

## Deep Dive (Sukellus syvemmälle):
Nykyhetken päivämäärän ja ajan saanti C-kielessä juontaa juurensa `time.h` kirjastoon, joka ilmestyi C:n standardikirjastoon 70-luvulla. Vaihtoehtojakin toki on: voit käyttää POSIX `gettimeofday` funktiota tai C11:n `timespec_get`. Implementaation ymmärtäminen auttaa välttämään aikavyöhyke- ja kesäaikavirheitä – `localtime` huomioi ne puolestasi.

## See Also (Katso myös):
- C Standard Library: https://en.cppreference.com/w/c/chrono
- Linux Manual page for `time(2)`: https://man7.org/linux/man-pages/man2/time.2.html
- Linux Manual page for `localtime(3)`: https://man7.org/linux/man-pages/man3/localtime.3.html
- Stack Overflow discussion about retrieving the current date and time in C: https://stackoverflow.com/questions/5141960/get-the-current-time-in-c