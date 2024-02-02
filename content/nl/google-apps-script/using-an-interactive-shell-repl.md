---
title:                "Gebruik van een interactieve shell (REPL)"
date:                  2024-02-01T22:04:01.463376-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gebruik van een interactieve shell (REPL)"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/google-apps-script/using-an-interactive-shell-repl.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een interactieve shell, of Read-Eval-Print Loop (REPL), is een eenvoudige, interactieve programmeeromgeving die individuele gebruikersinvoer (expressies) neemt, deze evalueert en het resultaat aan de gebruiker teruggeeft. Programmeurs gebruiken REPL's voor snelle prototyping, debugging en om de syntaxis en het gedrag van een programmeertaal interactief te leren kennen.

## Hoe te:

Google Apps Script, een op de cloud gebaseerde scripttaal voor het automatiseren van taken over Google-producten, heeft geen ingebouwde REPL-tool vergelijkbaar met die in talen zoals Python of JavaScript's Node.js. Je kunt echter een soortgelijke ervaring simuleren met behulp van de log- en debugfuncties van de Apps Script Editor of door een externe omgeving op te zetten. Hier richten we ons op het creëren van een geïmproviseerde REPL binnen de Apps Script editor.

1. **Een geïmproviseerde REPL-functie maken**:

```javascript
function myREPL() {
  var input = Logger.log('Voer je expressie in: ');
  try {
    var result = eval(input);
    Logger.log('Resultaat: ' + result);
  } catch(e) {
    Logger.log('Fout: ' + e.message);
  }
}
```

Aangezien directe gebruikersinvoer niet haalbaar is op dezelfde manier als een traditionele REPL in de Apps Script-omgeving, kun je de `input` variabele handmatig wijzigen en `myREPL()` uitvoeren om expressies te testen.

2. **Voorbeeldcode Uitvoeren**:

Stel dat je `2+2` wilt evalueren. Je zou dan de functie `myREPL` als volgt wijzigen:

```javascript
function myREPL() {
  var input = '2+2'; // Voer hier handmatig je expressie in
  // De rest blijft hetzelfde...
}
```

Na `myREPL()` uitgevoerd te hebben, controleer de Logs (Weergave > Logs) voor de output, die zoiets zou moeten lezen als:

```
[20-xx-xxxx xx:xx:xx:xxx] Voer je expressie in:
[20-xx-xxxx xx:xx:xx:xxx] Resultaat: 4
```

3. **Debuggen met Logger**:

Voor complexere debugging, verspreid `Logger.log(variabele);` binnen je code om variabele staten af te drukken, wat je helpt de flow en tussenstaten van je scripts te begrijpen.

## Diepere Duik

Het concept van een REPL is diep geworteld in de geschiedenis van het computergebruik, voortkomend uit de timesharing-systemen van de jaren 60 die interactieve sessies mogelijk maakten. Talen zoals Lisp gedijden in deze omgeving, aangezien de REPL cruciaal was voor hun iteratieve ontwikkelingsproces. In tegenstelling, Google Apps Script, dat veel later is ontstaan, is voornamelijk ontworpen voor het web, met een focus op het automatiseren van taken binnen Google's suite in plaats van iteratieve, op de console gebaseerde programmering.

Google Apps Script ondersteunt traditioneel geen real-time, interactieve codingsessies rechtstreeks vanwege zijn cloudgebaseerde aard en de focus op webapp-deployments. Zijn uitvoeringsmodel draait om functies die worden geactiveerd door webgebeurtenissen, tijdgestuurde triggers of handmatige oproepen binnen de omgeving, in plaats van de onmiddellijke feedbackloops die een REPL biedt.

Hoewel de geïmproviseerde REPL en debugger binnen de Apps Script Editor enig niveau van interactiviteit bieden, repliceren ze niet volledig de onmiddellijke feedback en efficiëntie van traditionele REPL's die in veel programmeertalen worden gevonden. Ontwikkelaars die op zoek zijn naar een authentiekere REPL-ervaring met Google-technologieën, zouden externe JavaScript-omgevingen of Node.js met Google's API's kunnen verkennen. Deze kunnen een responsievere en interactievere codingsessie bieden, zij het met meer opzet en mogelijk buiten de directe Apps Script-omgeving.
