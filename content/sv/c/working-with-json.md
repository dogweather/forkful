---
title:                "Att arbeta med json"
html_title:           "C: Att arbeta med json"
simple_title:         "Att arbeta med json"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/working-with-json.md"
---

{{< edit_this_page >}}

Vad & varför?
Att arbeta med JSON innebär att man hanterar data som är formaterade som en sträng. Det är vanligt att programmerare använder JSON för att enkelt kunna dela och läsa in data från tredjepartsapplikationer eller webbtjänster.

Hur gör man?
Det finns många vägar att genomföra JSON-hantering i C. Du kan till exempel använda externa bibliotek som json-c eller Jansson. Men om du vill hantera JSON-data på ett snabbt och enkelt sätt kan du använda C's inbyggda bibliotek "jsmn". Nedan följer ett exempel på hur man kan läsa in JSON-data från en fil och skriva ut den i terminalen:

```C
#include <stdio.h>
#include "jsmn.h" // inkludera jsmn-biblioteket

int main() {
  FILE* fp = fopen("example.json", "r"); // öppna json-filen
  fseek(fp, 0, SEEK_END); // gå till slutet på filen
  long file_size = ftell(fp); // få filstorlek
  fseek(fp, 0, SEEK_SET); // gå tillbaka till början av filen
  char* json_str = (char*)malloc(file_size * sizeof(char)); // allokera minne för json-strängen
  fread(json_str, sizeof(char), file_size, fp); // läs in json-data från filen och spara i strängen
  fclose(fp); // stäng filen

  jsmn_parser p;
  jsmntok_t t[128]; // skapa token-array för max 128 tokens
  jsmn_init(&p); // initiera parsern
  int r = jsmn_parse(&p, json_str, strlen(json_str), t, sizeof(t)/sizeof(t[0])); // parsra json-data och spara tokens i arrayen
  if (r < 0) { // kolla om något gick fel vid parsningen
    printf("Fel vid parsning av JSON: %d\n", r);
    return 1;
  }

  for (int i = 0; i < r; i++) { // loopa genom alla tokens
    if (t[i].type == JSMN_STRING) { // kolla om token är en sträng
      char* token_str = strndup(json_str + t[i].start, t[i].end - t[i].start); // skapa sträng av token
      printf("%s\n", token_str); // skriv ut strängen
      free(token_str); // frigör minne för strängen
    }
  }

  free(json_str); // frigör minne för json-strängen
  return 0;
}
```

Djupdykning:
JSON, eller JavaScript Object Notation, har funnits sedan början av 2000-talet och är en populär standard för att representera strukturerad data. I C finns det flera alternativ för att hantera JSON, men på grund av språkets enkelhet och snabbhet är det vanligt att programmerare väljer att göra det utan externa bibliotek. Istället använder de inbyggda funktioner som strängmanipulering och array-indexering för att läsa och hantera JSON-data.

Se även:
- https://github.com/json-c/json-c
- https://github.com/akheron/jansson