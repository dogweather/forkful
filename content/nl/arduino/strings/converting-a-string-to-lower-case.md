---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:34.290973-07:00
description: 'Hoe: Het `String` object van Arduino heeft een handige methode `toLowerCase()`.
  Roep deze op je string aan, en voila, het staat in kleine letters.'
lastmod: '2024-03-13T22:44:51.059715-06:00'
model: gpt-4-0125-preview
summary: Het `String` object van Arduino heeft een handige methode `toLowerCase()`.
title: Een string omzetten naar kleine letters
weight: 4
---

## Hoe:
Het `String` object van Arduino heeft een handige methode `toLowerCase()`. Roep deze op je string aan, en voila, het staat in kleine letters.

```Arduino
void setup() {
  Serial.begin(9600);
  String bericht = "Hallo, Wereld!";
  bericht.toLowerCase();
  Serial.println(bericht);  // Outputs: hallo, wereld!
}

void loop() {
  // Niets te doen hier.
}
```
Start je Seriële Monitor op, en je zult "hallo, wereld!" uitgeprint zien.

## Diepere Duik
Historisch gezien omvatte het omgaan met tekst vaak rekening houden met hoofd- en kleine letters. Gegevensinvoer, zoek- en sorteerbewerkingen negeren meestal hoofdletters om gebruikersfouten te verminderen en de robuustheid te vergroten. In andere talen, zoals C, zou je over elk karakter itereren en ze individueel omzetten met standaardbibliotheekfuncties. In de wereld van Arduino omhullen `String` objecten deze functionaliteit voor gebruiksgemak.

Alternatieven? Zeker. Je zou `toLowerCase()` kunnen gebruiken voor een `char` array, maar dan moet je door elk karakter lopen en het omzetten met `tolower()` uit `<ctype.h>`. Als je je zorgen maakt om geheugen en prestaties, overweeg dan om karakterarrays te gebruiken in plaats van `String` objecten en neem de controle met je eigen logica voor het omzetten naar kleine letters.

## Zie Ook
- Arduino's `String` referentiepagina: https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/
- C++ `<cctype>` bibliotheek voor karakterbewerkingen: http://www.cplusplus.com/reference/cctype/
- Voor een begrip van hoe stringvergelijking werkt en waarom het negeren van hoofdletters belangrijk kan zijn, zie: https://en.wikipedia.org/wiki/String_(computer_science)#Comparison
