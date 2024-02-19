---
aliases:
- /no/c/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:13:36.850625-07:00
description: "YAML, som st\xE5r for \"YAML Ain't Markup Language,\" er en menneskelesbar\
  \ standard for data-serialisering som kan brukes til alle slags applikasjoner, fra\u2026"
lastmod: 2024-02-18 23:08:54.415339
model: gpt-4-0125-preview
summary: "YAML, som st\xE5r for \"YAML Ain't Markup Language,\" er en menneskelesbar\
  \ standard for data-serialisering som kan brukes til alle slags applikasjoner, fra\u2026"
title: "\xC5 Arbeide med YAML"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

YAML, som står for "YAML Ain't Markup Language," er en menneskelesbar standard for data-serialisering som kan brukes til alle slags applikasjoner, fra konfigurasjonsfiler til datalagring. Programmerere jobber ofte med YAML når de trenger et format som er lett å lese og skrive for konfigurasjonsfiler eller datautveksling mellom språk og systemer.

## Hvordan:

Å jobbe med YAML i C krever et bibliotek, ettersom standard C-biblioteket ikke gir direkte støtte for YAML-parsing eller serialisering. Et av de mest populære YAML-bibliotekene for C er `libyaml`, som tilbyr både lavnivå- og høynivå-grensesnitt for parsing og generering av YAML. Nedenfor er et eksempel på hvordan man parser en enkel YAML-fil ved hjelp av `libyaml`:

**Først**, du må installere `libyaml`-biblioteket. Hvis du bruker et Unix-lignende system, kan du vanligvis installere det via pakkebehandleren din. For eksempel, på Ubuntu:

```bash
sudo apt-get install libyaml-dev
```

**Deretter**, se på en enkel YAML-fil med navn `config.yaml`:

```yaml
name: John Doe
age: 29
married: false
```

**Her er** et grunnleggende eksempel på hvordan du parser denne YAML-filen i C:

```c
#include <yaml.h>
#include <stdio.h>
#include <stdlib.h>

void process_yaml_file(const char *filename) {
    FILE *fh = fopen(filename, "rb");
    yaml_parser_t parser;
    yaml_event_t event;

    if (!yaml_parser_initialize(&parser))
        fputs("Klarte ikke å initialisere YAML-parser!\n", stderr);

    if (fh == NULL)
        fputs("Klarte ikke å åpne fil!\n", stderr);

    yaml_parser_set_input_file(&parser, fh);

    while (1) {
        if (!yaml_parser_parse(&parser, &event))
            break;

        if (event.type == YAML_SCALAR_EVENT) {
            printf("Verdi: %s\n", event.data.scalar.value);
        }

        if (event.type == YAML_STREAM_END_EVENT)
            break;

        yaml_event_delete(&event);
    }

    yaml_parser_delete(&parser);
    fclose(fh);
}

int main() {
    process_yaml_file("config.yaml");
    return 0;
}
```

Dette enkle programmet åpner en YAML-fil, initialiserer YAML-parseren, og leser filen, og skriver ut skalarverdiene (i dette eksempelet, feltene i vår enkle YAML). Merk at feilhåndtering er minimal i dette enkle eksempelet og bør være mer robust i produksjonskode.

Å kjøre programmet med vår `config.yaml` vil gi følgende utdata:

```plaintext
Verdi: John Doe
Verdi: 29
Verdi: false
```

## Dypdykk

YAML ble først utgitt i 2001 og var designet for å være mer lesbar og brukervennlig enn andre data-serialiseringsformater som XML eller JSON, låner fra flere språk som C, Perl, og Python for sin designfilosofi. Til tross for fordelene med lesbarhet og lette av menneskelig modifisering, kan YAML være komplekst å parse programmatisk på grunn av sin avhengighet av innrykk og et omfattende sett med funksjoner, inkludert referanser og egendefinerte typer.

Selv om `libyaml` gir robust, lavnivåtilgang til parsing og generering av YAML i C, kan det være tungvint for enkle oppgaver på grunn av sitt verbose API. Av disse grunnene foretrekker noen programmerere å bruke biblioteker på høyere nivå eller til og med andre data-serialiseringsformater som JSON når de jobber i C, spesielt når effektiv parsing med minimal kodeoverhead er en prioritet. Imidlertid forblir YAML et populært valg for konfigurasjonsfiler og situasjoner hvor menneskelig lesbarhet er av største betydning. Alternativer som TinyYAML eller innebygging av en tolk på høyere nivå (f.eks. innebygging av Python eller Lua) kan gi mer bekvemmelighet for spesifikke applikasjoner, balansere mellom brukervennlighet og ytelsesbehov.
