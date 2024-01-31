---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Kirjoittaminen standardivirheeseen (stderr) tarkoittaa viestienvälitystä ohjelman virheilmoituksia varten. Ohjelmoijat käyttävät tätä virheiden ja diagnostiikkatietojen esittämiseen, jolloin normaali tuloste (stdout) pysyy puhtaana tulostiedosta varten.

## How to: (Kuinka Tehdä:)
```C
#include <stdio.h>

int main() {
    fprintf(stderr, "Tapahtui virhe.\n");
    return 0;
}
```

Esimerkiksi yläpuolella stderriin kirjoitetaan viesti "Tapahtui virhe.". Tavallista tulostetta ei häiritä.

## Deep Dive (Syväluotaus)
Stderr on puskuroimaton, mikä tarkoittaa ettei viiveitä tapahdu viestien näyttämisessä. Historiallisesti stderr antoi keinoälyn välitöntä palautetta komentoriviltä juoksevalle ohjelmalle. Vaihtoehtona on lokitiedostoihin kirjoittaminen tai käyttäjän määrittämien virheilmoituskanavien hyödyntäminen. Stderr-kirjoittaminen on toteutettu C-kirjaston fprintf-funktiolla, jossa ensimmäinen argumentti on tiedostomuuttuja, joka viittaa stderr:iin.

## See Also (Katso Myös)
- C Standard Library Documentation: https://en.cppreference.com/w/c/io
- GNU C Library Reference Manual – Standard Streams: https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html
- Effective C: https://nostarch.com/Effective_C
