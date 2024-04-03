---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:59.592673-07:00
description: "Att arbeta med JSON (JavaScript Object Notation) i C inneb\xE4r att\
  \ tolka, generera och hantera JSON-datatstrukturer. Programmerare g\xF6r detta f\xF6\
  r att\u2026"
lastmod: '2024-03-13T22:44:38.406956-06:00'
model: gpt-4-0125-preview
summary: "Att arbeta med JSON (JavaScript Object Notation) i C inneb\xE4r att tolka,\
  \ generera och hantera JSON-datatstrukturer."
title: Att Arbeta med JSON
weight: 38
---

## Hur man gör:
För att arbeta med JSON i C kommer du vanligen att använda ett bibliotek som `jansson` eller `json-c` på grund av C:s brist på inbyggt stöd för JSON. Här kommer vi att fokusera på `jansson` för dess användarvänlighet och aktiva underhåll. Börja med att installera biblioteket (t.ex., genom att använda en pakethanterare som `apt` på Ubuntu: `sudo apt-get install libjansson-dev`).

Låt oss starta med att tolka en JSON-sträng och komma åt dess innehåll:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    const char *json_string = "{\"name\":\"John Doe\",\"age\":30}";
    json_error_t error;
    json_t *root = json_loads(json_string, 0, &error);
    
    if(!root) {
        fprintf(stderr, "error: on line %d: %s\n", error.line, error.text);
        return 1;
    }
    
    const char *name;
    int age;
    json_unpack(root, "{s:s, s:i}", "name", &name, "age", &age);
    
    printf("Namn: %s\nÅlder: %d\n", name, age);
    
    json_decref(root);
    return 0;
}
```

Exempelutskrift:
```
Namn: John Doe
Ålder: 30
```

Nästa, skapa och skriv ut ett JSON-objekt:

```c
#include <jansson.h>
#include <stdio.h>

int main() {
    json_t *root = json_object();
    json_object_set_new(root, "name", json_string("Jane Doe"));
    json_object_set_new(root, "age", json_integer(25));
    
    char *json_dump = json_dumps(root, JSON_ENCODE_ANY);
    printf("%s\n", json_dump);
    
    free(json_dump);
    json_decref(root);
    return 0;
}
```

Exempelutskrift:
```
{"name": "Jane Doe", "age": 25}
```

Dessa exempel demonstrerar grunderna i att ladda en JSON-sträng, packa upp dess värden, skapa ett nytt JSON-objekt och sedan skriva ut det som en sträng.

## Djupdykning
Behovet av att arbeta med JSON i C uppstår från webbens antagande av JSON som ett primärt format för datautbyte. JSON:s enkelhet och effektivitet gjorde att det snabbt övergick XML, trots C:s initiala frånvaro av direkt stöd för JSON-manipulation. Tidiga lösningar involverade manuell strängmanipulation - felbenäget och ineffektivt. Bibliotek som `jansson` och `json-c` framträdde för att fylla detta gap, och erbjuder robusta API:er för JSON-tolkning, konstruktion och serialisering.

Medan `jansson` erbjuder enkelhet och användarvänlighet, kan `json-c` locka de som söker en bredare uppsättning funktioner. Det är dock värt att notera att alternativ som tolkningsbibliotek i C++ erbjuder mer sofistikerade abstraktioner, tack vare detta språks mer komplexa datastrukturer och stöd från standardbiblioteket. Dock, när man arbetar i miljöer där C är det föredragna eller nödvändiga språket - som i inbyggda system eller när man interagerar med befintliga C-bibliotek - blir användningen av `jansson` eller `json-c` oumbärlig.

Det är även värt att notera att arbetet med JSON i C innebär en djupare förståelse för minneshantering, eftersom dessa bibliotek ofta returnerar dynamiskt allokerade objekt som kräver explicit deallokering. Detta utmanar programmerare att balansera bekvämlighet med ansvaret att förebygga minnesläckor, en avgörande aspekt av att utforma effektiv C-kod.
