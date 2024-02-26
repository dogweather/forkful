---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:13.178114-07:00
description: "Refactoring in de programmeertaal verwijst naar het proces van het herstructureren\
  \ van bestaande computercode\u2014het veranderen van de factoring zonder het\u2026"
lastmod: '2024-02-25T18:49:47.736079-07:00'
model: gpt-4-0125-preview
summary: "Refactoring in de programmeertaal verwijst naar het proces van het herstructureren\
  \ van bestaande computercode\u2014het veranderen van de factoring zonder het\u2026"
title: Refactoring
---

{{< edit_this_page >}}

## Wat & Waarom?

Refactoring in de programmeertaal verwijst naar het proces van het herstructureren van bestaande computercode—het veranderen van de factoring zonder het externe gedrag te wijzigen—om niet-functionele attributen te verbeteren. Het is een essentiële stap voor programmeurs om de leesbaarheid van de code te verhogen, complexiteit te verminderen en potentieel verborgen bugs naar boven te halen, wat gemakkelijkere onderhoud en toekomstige schaalbaarheid van de code bevordert.

## Hoe te:

In Google Apps Script is een veelvoorkomend scenario dat baat heeft bij refactoring het vereenvoudigen van omslachtige scripts die interageren met Google Sheets of Docs. Aanvankelijk kunnen scripts op een snelle en vuile manier worden geschreven om snel resultaten te behalen. Na verloop van tijd, als het script groeit, wordt het onhandelbaar. Laten we een voorbeeld doorlopen van refactoring voor betere leesbaarheid en efficiëntie.

**Origineel Script:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

Deze functie logt de naam van elk blad in een Google Spreadsheet. Hoewel het prima werkt, gebruikt het verouderde JavaScript-praktijken en ontbreekt het aan duidelijkheid.

**Gerefactoriseerd Script:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

In de gerefactoriseerde versie zijn we overgeschakeld op het gebruik van `const` voor variabelen die niet veranderen, waardoor onze bedoeling duidelijker wordt. We hebben ook de `forEach` methode gebruikt, een modernere en beknopte benadering om door arrays heen te gaan, wat de leesbaarheid verhoogt.

**Voorbeelduitvoer (voor beide scripts):**

De uitvoer in Logger zal er ongeveer zo uitzien, ervan uitgaande dat je Google Sheets document twee bladen heeft genaamd "Uitgaven" en "Inkomsten":

```
[20-04-2023 10:00:00: INFO] Uitgaven
[20-04-2023 10:00:01: INFO] Inkomsten
```

Het gerefactoriseerde script bereikt hetzelfde resultaat, maar is schoner en gemakkelijker te begrijpen in één oogopslag.

## Diepere duik

Refactoring in Google Apps Script erft gedeeltelijk zijn principes van de bredere software-engineeringpraktijk. Het werd meer erkend en gestructureerd als een concept in de late jaren '90, opvallend vanwege Martin Fowler's baanbrekende boek "Refactoring: Improving the Design of Existing Code" (1999), die een uitgebreide gids bood voor verschillende refactoringtechnieken. Hoewel de details van refactoring kunnen variëren tussen programmeertalen vanwege hun syntactische en functionele verschillen, blijft het kernobjectief hetzelfde: het verbeteren van code zonder zijn externe gedrag te veranderen.

In de context van Google Apps Script is een belangrijk aspect om tijdens het refactoren te overwegen de servicequota's en beperkingen die door Google worden opgelegd. Efficiënt gerefactoriseerde code leest niet alleen beter, maar draait ook sneller en betrouwbaarder binnen deze beperkingen. Bijvoorbeeld, batchbewerkingen (`Range.setValues()` in plaats van waarden één cel per keer instellen) kunnen de uitvoeringstijd en quotumconsumptie aanzienlijk verminderen.

Het is echter belangrijk op te merken dat voor bepaalde complexe projecten Google Apps Script tekort kan schieten vanwege deze zeer beperkingen. In dergelijke gevallen kan het kijken naar alternatieven zoals Google Cloud Functions of Apps Script's nieuwere broertje, AppSheet, betere schaalbaarheid en functionaliteit bieden.

Uiteindelijk, hoewel refactoring een kritieke vaardigheid is bij het onderhouden en verbeteren van Google Apps Script-projecten, is het begrijpen van de beperkingen van de omgeving en het overwegen van alternatieve oplossingen net zo belangrijk voor het leveren van efficiënte, robuuste en onderhoudbare code.
