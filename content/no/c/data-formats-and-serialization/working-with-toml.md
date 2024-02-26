---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:28.515710-07:00
description: "TOML (Toms Opplagte, Minimale Spr\xE5k) er et konfigurasjonsfilformat\
  \ som er lett \xE5 lese p\xE5 grunn av sine klare semantikk. Programmerere bruker\
  \ det for\u2026"
lastmod: '2024-02-25T18:49:39.475140-07:00'
model: gpt-4-0125-preview
summary: "TOML (Toms Opplagte, Minimale Spr\xE5k) er et konfigurasjonsfilformat som\
  \ er lett \xE5 lese p\xE5 grunn av sine klare semantikk. Programmerere bruker det\
  \ for\u2026"
title: "\xC5 jobbe med TOML"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

TOML (Toms Opplagte, Minimale Språk) er et konfigurasjonsfilformat som er lett å lese på grunn av sine klare semantikk. Programmerere bruker det for konfigurasjonsfiler i applikasjoner fordi dets enkelhet og menneskelesbarhet gjør det til et utmerket valg over formater som XML eller JSON i visse kontekster.

## Hvordan:

For å arbeide med TOML i C, trenger du først et bibliotek som er i stand til å parse TOML-filer, ettersom C standardbiblioteket ikke inkluderer denne funksjonaliteten. Et populært valg er `tomlc99`, en lettvekts TOML-parser for C99. Her er en rask veiledning for å lese en enkel TOML-konfigurasjonsfil:

Først, sørg for at du har installert `tomlc99` og riktig lenket det i prosjektet ditt.

**Eksempel på TOML-fil (`config.toml`):**
```toml
[database]
server = "192.168.1.1"
porter = [ 8001, 8001, 8002 ]
maks_tilkobling = 5000
aktivert = true
```

**C-kode for å parse denne filen:**

```c
#include <stdio.h>
#include <stdlib.h>
#include "toml.h"

int main() {
    FILE *konfigurasjonsFil;
    konfigurasjonsFil = fopen("config.toml", "r");
    if (!konfigurasjonsFil) {
        perror("Kan ikke åpne fil");
        return EXIT_FAILURE;
    }

    toml_table_t *konfig = toml_parse_file(konfigurasjonsFil, NULL, 0);
    if (!konfig) {
        fprintf(stderr, "Feil ved parsing av fil\n");
        fclose(konfigurasjonsFil);
        return EXIT_FAILURE;
    }

    toml_table_t *database = toml_table_in(konfig, "database");
    if (database) {
        const char *server = toml_raw_in(database, "server");
        printf("Databaseserver: %s\n", server);

        toml_array_t *porter = toml_array_in(database, "porter");
        for (int i = 0; i < toml_array_nelem(porter); i++) {
            int64_t port;
            toml_int_at(porter, i, &port);
            printf("Port %d: %ld\n", i, port);
        }
    }

    toml_free(konfig);
    fclose(konfigurasjonsFil);
    return EXIT_SUCCESS;
}
```

**Output:**
```
Databaseserver: "192.168.1.1"
Port 0: 8001
Port 1: 8001
Port 2: 8002
```

## Dypdykk

TOML ble skapt av Tom Preston-Werner, medgrunnlegger av GitHub, som en respons på de begrensningene han oppfattet i andre konfigurasjonsfilformater. Målet er å være rett frem og entydig, både for mennesker og datamaskiner, å lese og skrive uten å trenge komplekse parseringsregler. I C-økosystemet, er ikke TOML en førsteklasses borger som det kanskje ville vært i høyere nivå språk som for eksempel Rust med sin `serde_toml` eller Python med `toml`, som har biblioteker med innebygd støtte. Snarere må C-utviklere stole på eksterne biblioteker som `tomlc99`, men dette er typisk gitt Cs vekt på minimalisme og ytelse.

Selv om TOML er rost for sin klarhet, når man velger et konfigurasjonsfilformat, er det viktig å vurdere prosjektets behov. I scenarioer som krever mer komplekse strukturer eller interaktivitet med web-APIer, kan JSON eller til og med YAML tilby en bedre passform på tross av deres økte kompleksitet. TOML skinner i konfigurasjoner hvor lesbarhet og enkelhet er av største viktighet, ikke nødvendigvis der de mest avanserte datastrukturene trengs.
