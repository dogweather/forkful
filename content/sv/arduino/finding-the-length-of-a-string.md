---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att hitta längden på en sträng innebär att ta reda på hur många tecken en viss text består av. Detta är en användbar funktion för programmerare eftersom det tillåter dem att hantera och manipulera text på ett mer effektivt sätt.

## Hur man gör:

### Exempel 1:
```Arduino
String mittNamn = "Erika";
int längd = mittNamn.length();
Serial.println(längd); // 5
```

I detta exempel har vi en variabel som håller värdet "Erika". Genom att använda funktionen `.length()` kan vi ta reda på antalet bokstäver i namnet och tilldela det till variabeln `längd`. Sedan kan vi skriva ut längden på namnet på seriemonitorn med hjälp av `Serial.println()`.

### Exempel 2:
```Arduino
char meddelande[] = "Hej från Arduino!";
int längd = strlen(meddelande);
Serial.println(längd); // 17
```

I detta exempel har vi en array av tecken som representerar ett meddelande. Genom att använda funktionen `strlen()` från standardbiblioteket `string.h`, kan vi ta reda på längden på meddelandet och tilldela det till variabeln `längd`. Denna funktion fungerar liknande som `.length()` men för en array av tecken istället för en sträng.

## Fördjupning:

### Historisk kontext:
Att hitta längden på en sträng är en viktig del av textbearbetning inom programmering och har funnits sedan de första datorerna skapades. Då använde man dock oftast en manuell metod som kunde vara otillförlitlig och ineffektiv.

### Alternativ:
Utöver funktionerna `.length()` och `strlen()`, finns det också andra sätt att hitta längden på en sträng, t.ex. genom att använda en loop som räknar tecken en efter en.

### Implementeringsdetaljer:
Funktionen `.length()` förväntar sig en instans av `String` och returnerar ett heltal, medan `strlen()` förväntar sig en char-array och returnerar ett heltal av typen `size_t`.

## Se även:

- [Arduino Reference: String.length()](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/length/)
- [C Reference: strlen()](https://www.cplusplus.com/reference/cstring/strlen/)