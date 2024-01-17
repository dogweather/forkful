---
title:                "Å jobbe med json"
html_title:           "C: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/working-with-json.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
${DET} {å arbeide} med JSON er en viktig del av moderne programmering. JSON står for JavaScript Object Notation, og er et format for å lagre og utveksle data. {DET} er spesielt nyttig når {programmerere} trenger å sende data over internett eller lagre data i en fil. JSON er også lett å lese og skrive for både mennesker og datamaskiner.

## Hvordan: 
For å arbeide med JSON i C, kan du bruke et bibliotek som [cJSON](https://github.com/DaveGamble/cJSON). Her er et eksempel på hvordan du kan lage og behandle et JSON-objekt i C:

```C
#include <stdio.h>
#include "cJSON.h"

int main() {
  // Lager et nytt JSON-objekt
  cJSON *root = cJSON_CreateObject();
  // Legger til en nøkkel og verdi
  cJSON_AddStringToObject(root, "navn", "Ole");
  // Konverterer JSON-objektet til en streng
  char *json_str = cJSON_Print(root);
  // Skriver ut strengen
  printf("%s", json_str);
  // Frigjør minne
  free(json_str);
  cJSON_Delete(root);
  return 0;
}

// Output:
// {"navn":"Ole"}
```

## Dykk ned:
JSON ble opprinnelig utviklet av Douglas Crockford på slutten av 90-tallet som et alternativ til XML. Det har blitt et populært format for å lagre og utveksle data, særlig innen webutvikling. Det finnes også andre biblioteker for å arbeide med JSON i C, som [Jansson](https://github.com/akheron/jansson) og [jsmn](https://github.com/zserge/jsmn). Det er også viktig å huske på at sikkerhetsrisikoer kan oppstå når du behandler JSON-data, spesielt hvis de kommer fra eksterne kilder. Det er derfor viktig å validere og sanitisere dataene før du bruker dem i programmet ditt.

## Se også:
- [cJSON dokumentasjon](https://github.com/DaveGamble/cJSON/blob/master/README.md)
- [Jansson dokumentasjon](https://jansson.readthedocs.io/en/2.12/)
- [jsmn dokumentasjon](https://github.com/zserge/jsmn/blob/master/docs/usage.md)