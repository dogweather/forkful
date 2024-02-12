---
title:                "Att använda reguljära uttryck"
aliases:
- /sv/arduino/using-regular-expressions.md
date:                  2024-02-03T19:16:04.313979-07:00
model:                 gpt-4-0125-preview
simple_title:         "Att använda reguljära uttryck"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Reguljära uttryck (regex) är sekvenser av tecken som definierar sökmönster, främst använda för strängmatchning och manipulation. Programmerare använder regex i Arduino-projekt för att tolka seriell inmatning, validera användarinmatning eller extrahera data från strängar, vilket ökar effektiviteten och flexibiliteten i databehandlingen.

## Hur:
Arduino har inte inbyggt stöd för regex direkt i sitt standardbibliotek. Du kan dock uppnå regex-liknande funktionalitet för enkla mönster genom att använda grundläggande strängfunktioner, eller för mer komplexa behov, integrera ett tredjepartsbibliotek som `regex`.

### Enkel Strängmatchning utan Regex
För grundläggande behov, som att hitta en delsträng, kan du använda funktionen `String.indexOf()`:
```cpp
String data = "Sensorvärde: 12345";
int index = data.indexOf("värde:");
if (index != -1) {
  String värde = data.substring(index + 6).trim();
  Serial.println(värde); // Skriver ut: 12345
}
```

### Använda ett tredjepartsbibliotek för Regex
För att hantera mer komplexa mönster kan du överväga ett bibliotek som `regex`. Efter att ha installerat biblioteket kan du använda det enligt följande:

1. **Installation**: `regex`-biblioteket kanske inte finns tillgängligt i Arduino Library Manager, så du kan behöva installera det manuellt genom att ladda ner det från en pålitlig källa och lägga till det i din Arduino biblioteksmapp.

2. **Exempelanvändning**:
Med antagandet att biblioteket ger funktionalitet liknande standardimplementationer av regex, kan du använda det enligt följande:

```cpp
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial); // Vänta på att Serial ska vara redo
  
  regex_t reg;
  const char* mönster = "[0-9]+"; // Matchar en sekvens av siffror
  regcomp(&reg, mönster, REG_EXTENDED);
  
  const char* test_str = "Sensorvärde: 12345";
  
  regmatch_t matchningar[1];
  if (regexec(&reg, test_str, 1, matchningar, 0) == 0) {
    // Extrahera och skriv ut den matchande delen
    int start = matchningar[0].rm_so;
    int slut = matchningar[0].rm_eo;
    char match[slut-start+1];
    strncpy(match, test_str + start, slut-start);
    match[slut-start] = '\0';
    
    Serial.print("Hittade matchning: ");
    Serial.println(match); // Skriver ut: 12345
  } else {
    Serial.println("Ingen matchning hittades");
  }
  
  regfree(&reg); // Frigör det allokerade minnet för regex
}

void loop() {
  // placera din huvudkod här, för att köra upprepat:
}
```

**Notera**: Syntaxen och de specifika funktionerna som används här är för illustrativa ändamål och kan variera beroende på de faktiska implementationsdetaljerna för det `regex`-bibliotek du väljer. Hänvisa alltid till bibliotekets dokumentation för korrekt och aktuell information.
