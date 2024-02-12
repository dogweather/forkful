---
title:                "Gör om en sträng till versaler"
aliases:
- sv/arduino/capitalizing-a-string.md
date:                  2024-02-03T19:05:01.148231-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att skriva med stor bokstav innebär att konvertera det första tecknet i varje ord i en sträng till versal samtidigt som resten behålls som gemener. Denna operation är vanlig vid formatering av data och normalisering av användarinmatning för att bibehålla konsekvens och förbättra läsbarheten.

## Hur:
Arduino, som främst är känt för interaktion med hårdvara, inkluderar även grundläggande möjligheter för strängmanipulation genom sitt `String`-objekt. Dock saknas en direkt `capitalize`-funktion som finns i högre programmeringsspråk. Därför implementerar vi kapitalisering genom att iterera över en sträng och tillämpa bokstavsfallstransformationer.

Här är ett grundläggande exempel utan att använda tredjepartsbibliotek:

```cpp
String capitalizeString(String input) {
  if (input.length() == 0) {
    return ""; // Returnera en tom sträng om inmatningen är tom
  }
  input.toLowerCase(); // Omvandla hela strängen till gemener först
  input.setCharAt(0, input.charAt(0) - 32); // Gör första bokstaven versal
  
  // Gör bokstäver versala som följer efter ett mellanslag
  for (int i = 1; i < input.length(); i++) {
    if (input.charAt(i - 1) == ' ') {
      input.setCharAt(i, input.charAt(i) - 32);
    }
  }
  return input;
}

void setup() {
  Serial.begin(9600);
  String testStr = "hello arduino world";
  String capitalizedStr = capitalizeString(testStr);
  Serial.println(capitalizedStr); // Utdata: "Hello Arduino World"
}

void loop() {
  // Tom loop
}
```

Denna kodsnutt definierar en `capitalizeString`-funktion som först omvandlar hela strängen till gemener för att standardisera dess fall. Den gör sedan det första tecknet och varje tecken som följer ett mellanslag till versaler, vilket effektivt gör varje ord i inmatningssträngen till versaler. Observera att denna grundläggande implementation antar ASCII-teckenkodning och kan behöva justeringar för fullt stöd för Unicode.

För närvarande finns det inte allmänt antagna tredjepartsbibliotek specifikt för strängmanipulation i Arduinos ekosystem, huvudsakligen på grund av dess inriktning på hårdvaruinteraktion och effektivitet. Men det tillhandahållna exemplet är ett enkelt sätt att uppnå strängkapitalisering inom Arduinos programmeringsmiljö.
