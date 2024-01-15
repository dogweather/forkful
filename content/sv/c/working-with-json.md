---
title:                "Arbeta med json"
html_title:           "C: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-json.md"
---

{{< edit_this_page >}}

## Varför

JSON (JavaScript Object Notation) är en vanlig filformat som används för att lagra och överföra data. Det är snabbt, lättläst och platsbesparande, vilket gör det till ett populärt val för webbutveckling och andra applikationer. Genom att kunna arbeta med JSON i C kan du enkelt hantera och manipulera data för dina projekt och integrera med andra applikationer.

## Hur man gör

För att arbeta med JSON i C behöver du ett bibliotek som heter cJSON, som hjälper till att enkelt skapa, läsa och ändra data i JSON-format. Nedan följer ett exempel på hur du kan använda cJSON för att skapa ett JSON-objekt och skriva ut dess innehåll:

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cJSON.h"

int main() {
    // Skapar ett tomt cJSON-objekt
    cJSON *json = cJSON_CreateObject();

    // Lägger till ett nytt element
    cJSON_AddItemToObject(json, "namn", cJSON_CreateString("Lisa"));

    // Lägger till ett annat element med en array som värde
    cJSON_AddItemToObject(json, "frukter", cJSON_CreateStringArray(["äpple", "banan", "jordgubbe"]));

    // Skriver ut JSON-objektet i konsolen
    printf("%s\n", cJSON_Print(json));

    return 0;
}
```
**Resultat:**
```
{
    "namn": "Lisa",
    "frukter": [
        "äpple",
        "banan",
        "jordgubbe"
    ]
}
```

För att läsa data från en befintlig JSON-fil kan du använda funktionen `cJSON_ParseFile()` och sedan komma åt data genom att använda `cJSON_GetObjectItem()`. Här är ett exempel på hur du kan läsa en JSON-fil och skriva ut ett visst element i den:

```
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cJSON.h"

int main() {
    // Läser in en JSON-fil och skapar ett cJSON-objekt
    FILE *fp = fopen("exempel.json", "r");
    cJSON *json = cJSON_ParseFile(fp);
    fclose(fp);

    // Kommer åt ett element i JSON-objektet
    cJSON *element = cJSON_GetObjectItem(json, "namn");
    printf("Namn: %s\n", element->valuestring);

    return 0;
}
```
**Resultat:**
```
Namn: Lisa
```

Det finns många fler funktioner och möjligheter med cJSON-biblioteket, så det är värt att utforska det själv och experimentera med olika scenarion för att få en bättre förståelse av hur man kan hantera JSON i C.

## Djupdykning

Ett annat användbart sätt att arbeta med JSON i C är att konvertera mellan JSON och C-strängar. Detta kan göras med hjälp av `cJSON_Print()` och `cJSON_Parse()`. Det är också möjligt att skapa mer komplexa JSON-strängar med olika typer av värden och inbäddade objekt eller arrayer.

En annan utmärkt funktion i cJSON är `cJSON_StripWhitespace()`, som kan användas för att ta bort onödiga mellanslag och radbrytningar från en JSON-sträng. Detta är särskilt användbart om du behöver skicka eller mottaga JSON-data via en nätverksanslutning.

Ytterligare möjligheter med cJSON inkluderar att validera JSON-strängar och jämföra olika JSON-objekt för att se om de är lika.

## Se även

- cJSON-dokumentation: http://cjson.org
- C-appen för JSON-manipulering: https://github.com/DaveGamble/cJSON
- En korrekt formaterad JSON-fil: https://www.json.org/example.html