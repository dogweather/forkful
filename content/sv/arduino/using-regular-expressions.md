---
title:                "Använda reguljära uttryck"
html_title:           "Gleam: Använda reguljära uttryck"
simple_title:         "Använda reguljära uttryck"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Reguljära uttryck är en språkfunktion för att söka upp, matcha och hantera text på grundval av visst mönster. Programmerare använder reguljära uttryck för att effektivisera textbehandling och datamanipulering.

## Hur man gör:

Här är ett exempel på hur vi kan använda reguljära uttryck i Arduino-kod:
```Arduino
#include <regex.h>

void setup() {
  Serial.begin(9600);
  while (!Serial) {;}

  regex_t regex;
  int success;

  success = regcomp(&regex, "h", 0);
  if (success) {
    Serial.println("Failed to compile regex");
    return;
  }

  success = regexec(&regex, "hello", 0, NULL, 0);
  if (!success) {
    Serial.println("Match");
  } else if (success == REG_NOMATCH) {
    Serial.println("No match");
  } else {
    char errMsg[100];
    regerror(success, &regex, errMsg, sizeof(errMsg));
    Serial.println(String("Regex match failed: ") + errMsg);
  }

  regfree(&regex);
}

void loop() {}
```
Kodens output blir:
```
Match
```
## Djupdykning

Reguljära uttryck, eller regex, började som en funktion i Unix-textredigerare på 1970-talet. De erbjuder mycket mer än en enkel textsträngssökning, vilket gör att mycket komplex bearbetning av text och data kan koda på få rader.

Alternativ till regex i Arduino är inbyggda strängfunktionerna som `indexOf()`, `substring()`, `equals()`, osv. Även om dessa är bra för enkel textbehandling, kan de inte åstadkomma de mer komplexa mönstermatchningarna och manipuleringarna som regex kan.

Vid implementering av regex i Arduino är det viktigt att notera att det tar mycket mer minne än de inbyggda strängfunktionerna. Detta kan vara en begränsande faktor i minnes-sparande projekt.

## Se även

Ett antal bra källor för mer information om att använda reguljära uttryck inom Arduino inkluderar:
- [officiella Arduino-referenser](https://www.arduino.cc/reference/en/)
- ["Hävstångsreguljära uttryck för Arduino"](https://regexr.com/)
- [Inlärningsportal för Reguljära Uttryck](http://www.regular-expressions.info/tutorial.html)