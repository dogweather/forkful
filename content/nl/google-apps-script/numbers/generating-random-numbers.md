---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:18.980902-07:00
description: "Het genereren van willekeurige getallen is een fundamentele taak in\
  \ programmeren die wordt gebruikt voor een myriade aan toepassingen, zoals simulaties,\u2026"
lastmod: '2024-03-13T22:44:50.326640-06:00'
model: gpt-4-0125-preview
summary: "Het genereren van willekeurige getallen is een fundamentele taak in programmeren\
  \ die wordt gebruikt voor een myriade aan toepassingen, zoals simulaties,\u2026"
title: Willekeurige getallen genereren
---

{{< edit_this_page >}}

## Wat & Waarom?

Het genereren van willekeurige getallen is een fundamentele taak in programmeren die wordt gebruikt voor een myriade aan toepassingen, zoals simulaties, spellen en beveiligingssystemen. Programmeurs gebruiken deze techniek in Google Apps Script om variabiliteit te introduceren, scenario's te testen en onvoorspelbaarheid toe te voegen aan hun applicaties binnen het Google-ecosysteem, inclusief Sheets, Docs en Forms.

## Hoe te:

In Google Apps Script kun je willekeurige getallen genereren met de functie `Math.random()`, vergelijkbaar met JavaScript. Deze functie retourneert een zwevendekommagetel, pseudo-willekeurig getal in het bereik van 0 (inclusief) tot 1 (exclusief). Om deze getallen aan te passen voor verschillende gebruikssituaties, zoals het genereren van gehele getallen binnen een specifiek bereik, moet je mogelijk aanvullende berekeningen uitvoeren.

### Een Basis Willekeurig Getal Genereren

Om een eenvoudig willekeurig getal te genereren en dit in de console te loggen:

```javascript
function generateRandomNumber() {
  var randomNumber = Math.random();
  Logger.log(randomNumber);
}
```
*Voorbeelduitvoer:* `0.1234567890123456`

### Een Geheel Getal binnen een Specifiek Bereik Genereren

Om een willekeurig geheel getal tussen twee waarden (`min` en `max`) te genereren, inclusief:

```javascript
function getRandomInt(min, max) {
  min = Math.ceil(min);
  max = Math.floor(max);
  var randomNumber = Math.floor(Math.random() * (max - min + 1)) + min;
  Logger.log(randomNumber);
  return randomNumber;
}

// Voorbeeld:
getRandomInt(1, 10);
```
*Voorbeelduitvoer*: `7`

Onthoud, de `Math.ceil()` functie wordt gebruikt om de minimale waarde naar boven af te ronden, en `Math.floor()` wordt gebruikt om de maximale waarde naar beneden af te ronden, zodat het willekeurige getal binnen het gespecificeerde bereik valt.

## Diepgaand

Het mechanisme voor het genereren van willekeurige getallen in Google Apps Script, en inderdaad in de meeste programmeertalen, maakt gebruik van een pseudo-willekeurige getallengenerator (PRNG). Deze techniek is deterministisch en vertrouwt op een initiële waarde, bekend als de zaaier, om een reeks van getallen te produceren die willekeurig lijken. Hoewel voldoende voor veel toepassingen, is het belangrijk om te noteren dat pseudo-willekeurige getallen mogelijk niet geschikt zijn waar hoge beveiliging of echte willekeur vereist is, zoals in cryptografische toepassingen.

Echte willekeur kan worden bereikt door hardware willekeurige getallengeneratoren of diensten die willekeur genereren uit natuurlijke fenomenen. Echter, voor de meeste dagelijkse scriptingbehoeften in Google Apps Script, is `Math.random()` voldoende.

Historisch gezien heeft de zoektocht naar effectievere technieken voor het genereren van willekeurige getallen geleid tot de ontwikkeling van verschillende algoritmen, met als opvallende voorbeelden de Mersenne Twister en de Lineaire Congruentiële Generator (LCG). Gezien echter het hoge niveau van abstractie in Google Apps Script, hoeven de meeste gebruikers deze algoritmen niet direct te implementeren, maar het begrijpen van de onderliggende principes kan helpen bij het waarderen van het belang en de beperkingen van het genereren van willekeurige getallen in je scripts.
