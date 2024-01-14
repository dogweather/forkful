---
title:                "C: Arbeide med Yaml"
simple_title:         "Arbeide med Yaml"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-yaml.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvis du er en programmerer som er interessert i å legge til en fleksibel og lettleselig konfigurasjonsfil i prosjektet ditt, så er YAML noe du bør vurdere. Med sin enkle syntaks og støtte for flere programmeringsspråk, er YAML en flott løsning for å organisere og lagre konfigurasjonsdata.

# Hvordan

YAML er et lettvektig og intuitivt språk for å representere data. Det er basert på indentering og har støtte for nøkkelverdi-par, lister og innebygde datatyper som strenger, tall og booleans. La oss se på et enkelt eksempel på å representere konfigurasjonsdata i YAML:

```C
// Eksempel på en YAML-konfigurasjonsfil
server:
    name: localhost
    port: 8080
database:
    name: my_database
    username: admin
    password: password123
```

Som du kan se fra eksemplet over, er YAML veldig leselig og kan enkelt tolkes av både mennesker og maskiner. Når det kommer til å jobbe med YAML i C, kan du bruke biblioteket 'libyaml' for å analysere og generere YAML-data.

For å lese og behandle YAML-data i C, kan du følge disse trinnene:

1. Inkluder 'yaml.h' header-filen i koden din.
2. Åpne en YAML-konfigurasjonsfil og les filinnholdet inn i en 'yaml_parser_t' struktur.
3. Bruk 'yaml_parser_load()' funksjon for å analysere YAML-data fra parser-objektet.
4. Utforsk YAML-data ved hjelp av det nøstede 'yaml_event_t' objektet, som inneholder all informasjon om YAML-dataene (nøkkel, verdi, datastrukturer osv.).

Ta en titt på følgende eksempel for å se hvordan denne prosessen kan se ut i praksis:

```C
#include <yaml.h>

int main() {
    // Åpne en YAML-konfigurasjonsfil for lesing
    FILE *config_file = fopen("config.yaml", "r");

    // Initialiser en parser
    yaml_parser_t parser;
    if (!yaml_parser_initialize(&parser)) {
        // Håndter eventuelle feil
    }

    // Last inn YAML-data fra filen til parsingsobjektet
    yaml_parser_set_input_file(&parser, config_file);

    // Definer 'yaml_event_t' objektet for å utforske YAML-data
    yaml_event_t event;

    do {
        // Analyser neste hendelse i YAML-dataene
        if (!yaml_parser_parse(&parser, &event)) {
            // Håndter eventuelle feil
        }

        // Utforsk YAML-data basert på hendelsestype
        // Eventuelle nøstede hendelser kan behandles med en intern løkke
        switch (event.type) {
            case YAML_SCALAR_EVENT:
                // Behandle nøkkel/verdi-par
                printf("Key: %s | Value: %s\n", event.data.scalar.value, event.data.scalar.value);
                break;
            case YAML_SEQUENCE_START_EVENT:
                // Behandle starten av en liste
                printf("Start of a list\n");
                break;
            case YAML_MAPPING_START_EVENT:
                // Behandle starten av en map
                printf("Start of a map\n");
                break;
        }

        // Husk å frigjøre hendelsesobjektet etter å ha utforsket det
        yaml_event_delete(&event);
    } while (event.type != YAML_NO_EVENT);

    // Avslutt parseren og lukk YAML-filen
    yaml_parser_delete(&parser);
    fclose(config_file);

    return 0;
}
```

# Dypdykk

Et av de viktigste konseptene som er verdt å nevne når det kommer til å arbeide med YAML, er referanser. YAML har støtte for referanser som lar deg gjenbruke data i dokumentet ditt. Referanser er spesielt praktiske når du har flere ganger den samme nøkkelen i forskjellige deler av dokumentet.

La oss se på dette eksempelet for å få en bedre forståelse av referanser i YAML:

```C
server:
    name: localhost