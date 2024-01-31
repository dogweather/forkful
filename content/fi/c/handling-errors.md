---
title:                "Virheiden käsittely"
date:                  2024-01-26T00:36:52.971288-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheiden käsittely"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/handling-errors.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Virheiden käsittely C:ssä on odottamattoman odottamista. Se estää ohjelmia sekoamasta, kun ne kohtaavat ongelmia. Ohjelmoijat tekevät sen, jotta voivat käsitellä virheitä sulavasti ja pitääkseen koodinsa luotettavana.

## Kuinka tehdä:

Katsotaan, miten tämä tehdään C:ssä:

```C
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main() {
    FILE *fp = fopen("olematontiedosto.txt", "r");
    if (fp == NULL) {
        perror("Virhe tiedoston avaamisessa");
        return EXIT_FAILURE;
    }
    // Tee jotain tiedostolle
    fclose(fp);
    return EXIT_SUCCESS;
}
```

Esimerkkituloste, kun tiedostoa ei ole olemassa:
```
Virhe tiedoston avaamisessa: Tiedostoa tai hakemistoa ei ole
```

## Syväsukellus

Varhaisina C-päivinä virheiden hallinta oli perustavanlaatuista – lähinnä paluukoodeja ja manuaalisia tarkistuksia. Esittelyssä `errno`, globaali muuttuja, jota päivitetään kun funktiot epäonnistuvat. Se ei ole itsessään säieturvallinen, joten uudemmat `strerror`- ja `perror`-funktiot esiteltiin paremman virheraportoinnin aikaansaamiseksi.

Vaihtoehdot? Moderni C ei ole rajoittunut `errno`on. On olemassa setjmp ja longjmp ei-lokaaleihin hyppyihin, kun katastrofi iskee. Jotkut suosivat määrittelemään omia virhekoodejaan, kun taas toiset optioivat poikkeustyyppisiin rakenteisiin C++:ssa.

Toteutuksen yksityiskohdat voivat olla monimutkaisia. Esimerkiksi `errno` on säieturvallinen POSIX-yhteensopivissa järjestelmissä säielokaalisen tallennustilan (TLS) taian ansiosta. Sulautetuissa järjestelmissä, joissa resurssit ovat arvokkaita, saatetaan suosia räätälöityä virheidenkäsittelykoodia standardimenetelmien sijaan, jotka voivat paisuttaa ohjelmiston.

## Katso myös

- Yksityiskohtainen syventyminen `errno`on: https://en.cppreference.com/w/c/error/errno
- Säieturvallisuudesta, katso POSIX-säikeet ja errno: http://man7.org/linux/man-pages/man3/pthread_self.3.html
- Johdatus setjmp:iin ja longjmp:iin: https://www.cplusplus.com/reference/csetjmp/
- Poikkeusten käsittelystä C++:ssa, tarkista: https://isocpp.org/wiki/faq/exceptions
