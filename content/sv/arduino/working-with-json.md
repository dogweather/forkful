---
title:                "Arbeta med json"
html_title:           "Arduino: Arbeta med json"
simple_title:         "Arbeta med json"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/working-with-json.md"
---

{{< edit_this_page >}}

## Vad & Varför?
JSON är ett sätt att strukturera och spara data i ett format som är läsbart för både människor och datorer. Det används ofta av programmerare för att överföra och lagra data på ett effektivt sätt.

## Hur man gör:
### Koda en JSON-sträng:
```
ArduinoJsonBuffer buffer;  // skapa ett bufferobjekt
JsonObject& json = buffer.createObject(); // skapa ett jsonobjekt
json["namn"] = "Lisa"; // lägg till ett fält "namn" med värdet "Lisa"
json["ålder"] = 25; // lägg till ett fält "ålder" med värdet 25
String jsonStr; // skapa en sträng för att lagra JSON-data
json.printTo(jsonStr); // skriv jsonobjektet till strängen
```

### Konvertera en JSON-sträng till variabler:
```
const char* jsonStr = "{\"namn\":\"Lisa\",\"ålder\":25}"; // en json-sträng
StaticJsonBuffer<200> buffer; // skapa ett bufferobjekt
JsonObject& json = buffer.parseObject(jsonStr); // konvertera strängen till ett jsonobjekt
String name = json["namn"]; // spara fältet "namn" i en variabel
int age = json["ålder"]; // spara fältet "ålder" i en variabel
```

## Djupdykning:
### Historisk kontext:
JSON skapades 2001 av Douglas Crockford och har sedan dess blivit ett populärt sätt att hantera data. Det används ofta tillsammans med webbtjänster och API:er för att överföra data mellan olika program och system.

### Alternativ:
En populär konkurrent till JSON är XML, men JSON är vanligtvis mer lättläst och enklare att arbeta med. Andra alternativ inkluderar YAML och CBOR.

### Implementeringsdetaljer:
ArduinoJson biblioteket är en öppen källkodslösning för att enkelt skapa och behandla JSON-data i Arduino-program. Det finns olika versioner som kan anpassas efter projektets behov.

## Se även:
- [ArduinoJson library](https://arduinojson.org/)
- [C++ JSON library](https://github.com/nlohmann/json)
- [JSON - Wikipedia](https://sv.wikipedia.org/wiki/JSON)