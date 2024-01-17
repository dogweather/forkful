---
title:                "Sammenføyning av strenger"
html_title:           "Arduino: Sammenføyning av strenger"
simple_title:         "Sammenføyning av strenger"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Sammenføyning av strenger er når to eller flere strenger blir satt sammen for å lage en lengre streng. Dette er nyttig for programmerere når de trenger å kombinere tekst og variabler for å generere dynamisk innhold.

# Hvordan:
```
Arduino

Det er enkelt å utføre konkatenasjon av strenger på Arduino ved hjelp av operatoren "+=". Her er et eksempel på hvordan du kan kombinere tekst og en variabel for å lage en beskjed:

```
String navn = "Ada";
String beskjed = "Hei " + navn;
Serial.println(beskjed);
// Output: Hei Ada
```

Du kan også kombinere flere variabler sammen for å danne en lengre streng. Her er et eksempel:

```
int alder = 25;
String yrke = "ingeniør";
String beskjed = "Jeg er " + alder + " år gammel og jobber som " + yrke;
Serial.println(beskjed);
// Output: Jeg er 25 år gammel og jobber som ingeniør
```

Husk at du kan bruke konkatenasjon både med strenger og tall. Her er et eksempel på hvordan du kan kombinere en streng og et tall for å printe ut en beskjed:

```
int antall = 10;
String beskjed = "Du har " + String(antall) + " nye meldinger";
Serial.println(beskjed);
// Output: Du har 10 nye meldinger
```

# Dypdykk:
Konkatenasjon av strenger har vært en viktig del av programmering siden de tidlige dagene av programmeringsspråk. I Arduino kan du også bruke funksjonen `String.concat()` for å kombinere strenger. Det finnes også alternative metoder som innebærer å bruke arrays eller `sprintf()` funksjonen.

Det er viktig å huske på at når du bruker `String`-variabler og konkatenasjon, kan det føre til allokering av minne og øke bruken av dynamisk minnehåndtering, noe som kan føre til redusert ytelse. Derfor bør du prøve å bruke `char`-array til konkatenasjon hvis det er mulig.

# Se også:
- [Arduino String konkatenering](https://www.arduino.cc/en/Tutorial/StringAdditionOperator)
- [C++ Strings](https://www.w3schools.in/cplusplus/strings/)
- [Alternative måter å kombinere strenger på i Arduino](https://forum.arduino.cc/index.php?topic=237596.0)