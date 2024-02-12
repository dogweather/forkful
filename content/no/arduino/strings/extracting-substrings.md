---
title:                "Uthenting av delstrenger"
aliases:
- /no/arduino/extracting-substrings/
date:                  2024-01-20T17:44:57.663878-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uthenting av delstrenger"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å trekke ut substrings betyr å hente en spesiell del av en tekststreng. Programmerere gjør dette for å behandle eller analysere spesifikke datapartier innen en større tekst.

## Hvordan:
```Arduino
String fullTekst = "Heisann Sveisann!";
String delTekst;

// Hente begynnelsen av strengen
delTekst = fullTekst.substring(0, 7);
Serial.println(delTekst); // Skriver ut "Heisann"

// Hente en midtdel av strengen
delTekst = fullTekst.substring(8, 16);
Serial.println(delTekst); // Skriver ut "Sveisann"

// Hente slutten av strengen fra et startpunkt
delTekst = fullTekst.substring(8);
Serial.println(delTekst); // Skriver ut "Sveisann!"
```

## Dypdykk:
Å trekke ut substrings er en fundamental operasjon i mange programmeringsspråk, og Arduino gjør det enkelt gjennom `String`-klassen. Substrings har vært en del av programmering siden språk som C, hvor man skulle bruke funksjoner som `strncpy` for å oppnå samme resultat. I Arduino, som benytter C++-basert syntaks, gjør `String`-klassen jobben enklere. Alternativer inkluderer manipulering ved hjelp av `char`-arrays og pekere, men dette kan være mer krevende og feilutsatt. Detaljer rundt implementasjonen av substrings er viktig: Sørg for å håndtere grensetilfeller, som når start- eller sluttpunktet er utenfor strengens lengde.

## Se Også:
- Arduino's String-klasse dokumentasjon: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- En guide til C++ strenger for dybdeinformasjon: http://www.cplusplus.com/reference/string/string/substr/
- Alternative metoder med 'char' arrays: https://www.arduino.cc/reference/en/language/variables/data-types/chararray/
