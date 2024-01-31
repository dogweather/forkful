---
title:                "Jobbe med TOML"
date:                  2024-01-26T04:19:41.862107-07:00
model:                 gpt-4-0125-preview
simple_title:         "Jobbe med TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-toml.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
TOML er et dataserialiseringsspråk designet for å være lett å lese og skrive. Programmerere bruker det til konfigurasjonsfiler, enkel datalagring og utveksling av data på tvers av språk på grunn av dets klarhet og menneskevennlighet.

## Hvordan:
La oss parse en TOML-konfigurasjonsfil i C ved hjelp av "tomlc99"-biblioteket. Først, installer biblioteket. Deretter, opprett en `config.toml`:

```toml
title = "TOML Eksempel"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
```

Nå, parser den i C:

```c
#include <stdio.h>
#include "toml.h"

int main() {
    FILE* fp;
    char errbuf[200];

    if (0 == (fp = fopen("config.toml", "r"))) {
        printf("Feil: kan ikke åpne konfigurasjonsfilen\n");
        return 1;
    }
    
    toml_table_t* conf = toml_parse_file(fp, errbuf, sizeof(errbuf));
    fclose(fp);
    if (0 == conf) {
        printf("Feil: %s\n", errbuf);
        return 1;
    }

    printf("Tittel: %s\n", toml_raw_in(conf, "title"));

    toml_table_t* eier = toml_table_in(conf, "owner");
    printf("Eiernavn: %s\n", toml_raw_in(eier, "name"));

    toml_free(conf);
    return 0;
}
```
Eksempel på utdata:
```
Tittel: "TOML Eksempel"
Eiernavn: "Tom Preston-Werner"
```

## Dypdykk
TOML, som står for Toms Opplagte, Minimalistiske Språk, ble opprettet av Tom Preston-Werner i 2013. Det fungerer som et enklere alternativ til formater som XML og YAML, med fokus på å være mer menneske-lesbart og skrivbart. Selv om JSON er et annet alternativ, beholder TOML en struktur som er lettere å parse visuelt av mennesker, noe som er en av hovedgrunnene til dets adopsjon i konfigurasjonsfiler.

I C, arbeid med TOML involverer valg av et parserbibliotek siden språket ikke støtter det nativt. Biblioteker som "tomlc99" er C99-kompatible og tilbyr et API for å dekode TOML-tekst. Når man vurderer ytelse, er riktig feilhåndtering og minnehåndtering avgjørende da C ikke har innebygd søppelsamling.

## Se også:
1. TOML Spesifikasjon: [https://toml.io/en/](https://toml.io/en/)
2. tomlc99 GitHub-repo: [https://github.com/cktan/tomlc99](https://github.com/cktan/tomlc99)
3. Sammenligning av dataserialiseringsformater: [https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html](https://labs.eleks.com/2015/07/comparison-of-data-serialization-formats.html)
