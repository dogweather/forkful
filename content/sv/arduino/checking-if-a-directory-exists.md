---
title:                "Kontrollera om en mapp finns"
html_title:           "Arduino: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns är ett vanligt förfarande inom programmering. Det innebär helt enkelt att man undersöker om en specifik katalog existerar eller inte. Detta är användbart för att säkerställa att ens program fungerar korrekt och inte försöker att åtkomma en katalog som inte finns.

## Hur man:
Här är ett enkelt exempel på hur man kan kontrollera om en katalog med namnet "test" finns:

```Arduino
if (SD.exists("test")) {
  Serial.println("Katalogen finns.");
} else {
  Serial.println("Katalogen finns inte.");
}
```
Exempelutskrift:
```
Katalogen finns.
```
Ovanstående kod använder sig av Arduino SD-biblioteket för att kontrollera om katalogen "test" finns på SD-kortet.

## Djupdykning:
Att kontrollera om en katalog finns är en viktig del av filhanteringen i ett program. Det är en bra praxis att alltid kontrollera om en katalog finns innan man försöker att använda den för att undvika oförutsedda fel.

Det finns också olika sätt att kontrollera om en katalog existerar, som att använda funktioner som "isDirectory()" eller "existsDir()" beroende på vilket programmeringsspråk man använder. I det här exemplet används funktionen "exists()" från Arduino SD-biblioteket.

## Se även:
[Arduino SD Library Reference](https://www.arduino.cc/en/Reference/SD)

[How to check if directory exists in C++](https://stackoverflow.com/questions/12774207/how-to-check-if-directory-exists-in-c)