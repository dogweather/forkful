---
date: 2024-01-20 17:56:57.507359-07:00
description: "Hvordan: S\xF8k og erstatt funksjonaliteten har sine r\xF8tter i tidlige\
  \ tekstbehandlingsverkt\xF8y. Alternativer inkluderer regex (regular expressions)\
  \ for\u2026"
lastmod: '2024-04-05T21:53:42.005732-06:00'
model: gpt-4-1106-preview
summary: "S\xF8k og erstatt funksjonaliteten har sine r\xF8tter i tidlige tekstbehandlingsverkt\xF8\
  y."
title: "S\xF8king og erstatting av tekst"
weight: 10
---

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
