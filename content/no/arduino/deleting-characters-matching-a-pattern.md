---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å slette tegn som matcher et mønster handler om å identifisere spesifikke tegnkombinasjoner i tekst og fjerne dem. Dette er nyttig for å rydde opp i data, luke ut uønskede tegn og effektivisere behandlingen av informasjon.

## Hvordan gjøre det:

For å demonstrere, la oss lage en funksjon for å slette alle tall fra en tekststreng. Her er en kodebit som gjør jobben:

```Arduino
String tekst = "D123ette4 er 56en 789test";
String nyTekst = "";

for (int i = 0; i < tekst.length(); i++){
 if (!isDigit(tekst.charAt(i))){
   nyTekst += tekst.charAt(i);
 }
}

Serial.println(nyTekst);
```

Utdata vil være: "Dette er en test".

## Dypdykk:

Historisk sett, har funksjonen for sletting av tegn som matcher et mønster sin opprinnelse fra regulære uttrykk (RegEx), som ble introdusert allerede i 1950-tallet.

Det finnes alternativer til å bruke en for-løkke og isDigit-funksjonen som i eksemplet over. For eksempfel, avhengig av problemet du prøver å løse, kan du bruke innebygde funksjoner som String.replace() eller lage dine egne funksjoner for å fjerne tegn.

For å implementere funksjonaliteten over på en effektiv måte, er det viktig å huske på at strenger i C++ (som Arduino bruker) er uforanderlige. Dette betyr at hver gang du endrer en streng, lager du faktisk en ny. For store strenger eller i løkker kan dette føre til minneproblemer.

## Se Også:

[Introduksjon til RegEx](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)

[Arduino String Functions](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)

[Arduino Memory](https://www.arduino.cc/en/tutorial/memory)