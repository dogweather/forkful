---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:12:06.159840-07:00
description: "\xC5 jobbe med JSON (JavaScript Object Notation) i C inneb\xE6rer \xE5\
  \ parsing, generere og manipulere JSON datastrukturer. Programmerere gj\xF8r dette\
  \ for \xE5\u2026"
lastmod: '2024-03-13T22:44:41.295196-06:00'
model: gpt-4-0125-preview
summary: "\xC5 jobbe med JSON (JavaScript Object Notation) i C inneb\xE6rer \xE5 parsing,\
  \ generere og manipulere JSON datastrukturer."
title: Arbeide med JSON
weight: 38
---

## Hva & Hvorfor?

Å jobbe med JSON (JavaScript Object Notation) i C innebærer å parsing, generere og manipulere JSON datastrukturer. Programmerere gjør dette for å muliggjøre kommunikasjon med webtjenester, datalagring eller konfigurasjonsfiler i et lettvekt og menneskelesbart format.

## Hvordan:

For å jobbe med JSON i C, vil du vanligvis bruke et bibliotek som `jansson` eller `json-c` på grunn av Cs mangel på innebygd støtte for JSON. Her vil vi fokusere på `jansson` på grunn av dets brukervennlighet og aktive vedlikehold. Først, installer biblioteket (f.eks., ved å bruke en pakkehåndterer som `apt` på Ubuntu: `sudo apt-get install libjansson-dev`).

La oss starte med å parse en JSON-streng og få tilgang til innholdet:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if (!root) {
        fprintf(stderr, "feil: på linje %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Navn: %s\nAlder: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

Eksempelutdata:
```
Navn: John Doe
Alder: 30
```

Videre, å opprette og skrive ut et JSON-objekt:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    gratis(json_dump);
    json_decref(root);
    return 0;
}
```

Eksempelutdata:
```
{"name": "Jane Doe", "age": 25}
```

Disse eksemplene demonstrerer grunnleggende om å laste en JSON-streng, pakke ut verdiene, opprette et nytt JSON-objekt, og deretter skrive det ut som en streng.

## Dypdykk

Behovet for å jobbe med JSON i C oppstår fra webbens adopsjon av JSON som et primært format for datautveksling. JSONs enkelhet og effektivitet gjorde at den raskt forbigikk XML, til tross for Cs opprinnelige fravær i direkte støtte for JSON-manipulasjon. Tidlige løsninger involverte manuell strengmanipulasjon - feilutsatt og ineffektivt. Biblioteker som `jansson` og `json-c` oppsto for å fylle dette gapet, og tilbyr robuste APIer for parsing, konstruksjon og serialisering av JSON.

Selv om `jansson` tilbyr enkelhet og brukervennlighet, kan `json-c` tiltrekke de som ser etter et bredere funksjonssett. Likevel, alternativer som parsingbiblioteker i C++ tilbyr mer sofistikerte abstraksjoner, takket være det språkets mer komplekse datastrukturer og støtte fra standardbiblioteket. Imidlertid, når man jobber i miljøer der C er det foretrukne eller nødvendige språket - som i innebygde systemer eller når man skal grensesnitt med eksisterende C-biblioteker - blir bruk av `jansson` eller `json-c` uunnværlig.

Det er også verdt å nevne at arbeid med JSON i C innebærer en dypere forståelse av minnehåndtering, da disse bibliotekene ofte returnerer dynamisk allokerte objekter som krever eksplisitt deallokering. Dette utfordrer programmerere til å balansere bekvemmelighet med ansvaret for å forhindre minnelekkasjer, en avgjørende del av å utforme effektiv C-kode.
