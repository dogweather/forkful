---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:04:43.012975-07:00
description: "Reguliere Expressies (regex) zijn patronen die worden gebruikt om combinaties\
  \ van karakters in strings te matchen. Programmeurs benutten ze voor het\u2026"
lastmod: '2024-02-25T18:49:47.717045-07:00'
model: gpt-4-0125-preview
summary: "Reguliere Expressies (regex) zijn patronen die worden gebruikt om combinaties\
  \ van karakters in strings te matchen. Programmeurs benutten ze voor het\u2026"
title: Reguliere expressies gebruiken
---

{{< edit_this_page >}}

## Wat & Waarom?

Reguliere Expressies (regex) zijn patronen die worden gebruikt om combinaties van karakters in strings te matchen. Programmeurs benutten ze voor het zoeken, bewerken of manipuleren van tekst en data, waardoor ze onmisbaar zijn voor patroonherkenning en data-analyse taken.

## Hoe:

Het gebruik van reguliere expressies in Google Apps Script is eenvoudig dankzij de syntax gebaseerd op JavaScript. Hier is hoe je regex kunt integreren in je scripts voor veelvoorkomende taken zoals zoeken en datavalidatie.

### Zoeken in Strings

Stel je wilt vinden of een string een specifiek patroon bevat, zoals een e-mailadres. Hier is een simpel voorbeeld:

```javascript
function findEmailInText(text) {
  var emailPattern = /\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}\b/;
  var found = text.match(emailPattern);
  if (found) {
    Logger.log("Gevonden: " + found[0]);
  } else {
    Logger.log("Geen email gevonden.");
  }
}

// Voorbeeldgebruik
findEmailInText("Neem contact met ons op via info@example.com.");
```

### Data Validatie

Reguliere expressies schitteren in datavalidatie. Hieronder staat een functie die een invoerstring valideert om te controleren of deze voldoet aan een eenvoudig wachtwoordbeleid (minimaal één hoofdletter, één kleine letter, en minimaal 8 karakters).

```javascript
function validatePassword(password) {
  var passwordPattern = /^(?=.*[a-z])(?=.*[A-Z]).{8,}$/;
  return passwordPattern.test(password);
}

// Voorbeelduitvoer
Logger.log(validatePassword("Str0ngPass")); // Uitvoer: true
Logger.log(validatePassword("zwak"));       // Uitvoer: false
```

## Diepgaand

Reguliere expressies in Google Apps Script zijn overgeërfd van JavaScript, voor het eerst gestandaardiseerd in de ECMAScript-taalspecificatie in juni 1997. Hoewel krachtig, kunnen ze soms leiden tot verwarrende en moeilijk te onderhouden code, vooral wanneer ze overmatig gebruikt worden of voor complexe patroonmatchingtaken die mogelijk efficiënter opgelost kunnen worden door andere parsingmethoden.

Bijvoorbeeld, hoewel je regex kunt gebruiken voor HTML- of XML-parsing in een noodgeval, wordt dit over het algemeen afgeraden vanwege de geneste en ingewikkelde structuren van deze documenten. In plaats daarvan zijn tools die specifiek ontworpen zijn voor het parsen van dergelijke structuren, zoals DOM-parsers voor HTML, betrouwbaarder en leesbaarder.

Bovendien moeten Google Apps Script-ontwikkelaars bedachtzaam zijn op potentiële prestatieproblemen bij het gebruik van complexe regex-patronen bij grootschalige tekstmanipulatietaken, aangezien regex-verwerking CPU-intensief kan zijn. In dergelijke gevallen kan het opsplitsen van de taak in eenvoudigere deeltaken of het gebruik van ingebouwde stringmanipulatiefuncties een betere balans bieden tussen prestatie en onderhoud.
