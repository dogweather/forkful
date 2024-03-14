---
date: 2024-01-20 17:56:57.507359-07:00
description: "Teksts\xF8k og -erstattning lar deg finne spesifikke ord eller fraser\
  \ og endre dem til noe annet. Programmerere bruker dette for \xE5 oppdatere kode,\
  \ korrigere\u2026"
lastmod: '2024-03-13T22:44:41.044150-06:00'
model: gpt-4-1106-preview
summary: "Teksts\xF8k og -erstattning lar deg finne spesifikke ord eller fraser og\
  \ endre dem til noe annet. Programmerere bruker dette for \xE5 oppdatere kode, korrigere\u2026"
title: "S\xF8king og erstatting av tekst"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Tekstsøk og -erstattning lar deg finne spesifikke ord eller fraser og endre dem til noe annet. Programmerere bruker dette for å oppdatere kode, korrigere feil eller endre data dynamisk.

## Hvordan:
```Arduino
String tekst = "Hilsen fra Arduino verdenen!";
String erstattetTekst = tekst.replace("verdenen", "universet");
Serial.begin(9600);
Serial.println(erstattetTekst); // Skriver ut "Hilsen fra Arduino universet!"
```

## Dypdykk:
Søk og erstatt funksjonaliteten har sine røtter i tidlige tekstbehandlingsverktøy. Alternativer inkluderer regex (regular expressions) for komplekse mønstre, eller direkte manipulasjon av strengtegn. På Arduino implementerer du dette ved å bruke `String`-klassens `.replace()` metode for enkel tekstmanipulasjon.

## Se Også:
- Arduino `String` klasse referanse: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- Regex guide: https://www.regular-expressions.info/
- Tekstbehandling med Arduino: https://create.arduino.cc/projecthub/projects/tags/text
