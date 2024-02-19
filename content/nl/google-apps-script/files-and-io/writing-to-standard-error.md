---
aliases:
- /nl/google-apps-script/writing-to-standard-error/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:09:38.210439-07:00
description: "Schrijven naar standaardfout (stderr) in programmeertalen gaat over\
  \ het richten van foutmeldingen en diagnostiek naar een aparte stroom, los van de\u2026"
lastmod: 2024-02-18 23:09:01.404530
model: gpt-4-0125-preview
summary: "Schrijven naar standaardfout (stderr) in programmeertalen gaat over het\
  \ richten van foutmeldingen en diagnostiek naar een aparte stroom, los van de\u2026"
title: Schrijven naar standaardfout
---

{{< edit_this_page >}}

## Wat & Waarom?

Schrijven naar standaardfout (stderr) in programmeertalen gaat over het richten van foutmeldingen en diagnostiek naar een aparte stroom, los van de standaarduitvoer (stdout). Programmeurs doen dit om normale programma-uitvoer van foutmeldingen te scheiden, wat het debuggen en loganalyse eenvoudiger maakt.

## Hoe te:

Google Apps Script, een scripttaal voor lichtgewicht applicatieontwikkeling op het Google Apps-platform, biedt geen directe ingebouwde functie zoals `console.error()` voor het schrijven naar stderr, zoals je zou vinden in Node.js of Python. Echter, je kunt dit gedrag simuleren door gebruik te maken van Google Apps Script's loggingdiensten of aangepaste foutafhandeling om foutuitvoeren te beheren en te scheiden.

### Voorbeeld: Gebruik van `Logger` voor Foutmeldingen

```javascript
function logError() {
  try {
    // Simuleer een fout
    const result = 1 / 0;
    if(!isFinite(result)) throw new Error("Poging tot deling door nul");
  } catch (e) {
    // Schrijf foutmelding naar Logs
    Logger.log('Fout: ' + e.message);
  }
}
```

Wanneer je `logError()` uitvoert, schrijft dit de foutmelding naar de log van Google Apps Script, die je kunt bekijken door `Weergave > Logs`. Dit is niet precies stderr, maar het dient een soortgelijk doel van het scheiden van foutlogs van standaarduitvoeren.

### Geavanceerde Diagnostische Logging

Voor geavanceerdere debugging en foutlogging, kun je Stackdriver Logging gebruiken, nu bekend als Google Cloud's Operations Suite.

```javascript
function advancedErrorLogging() {
  try {
    // Bewust een fout veroorzaken
    const obj = null;
    const result = obj.someProperty;
  } catch (e) {
    console.error('Fout ontdekt: ', e.toString());
  }
}
```

Dit stuurt de foutmelding naar Stackdriver Logging, waar het wordt beheerd als een foutniveau-log. Let erop dat de integratie van Stackdriver/Google Cloud’s Operations Suite een fijnere en doorzoekbarere logoplossing biedt in vergelijking met `Logger`.

## Diepere duik

Het ontbreken van een toegewijde `stderr` stroom in Google Apps Script weerspiegelt zijn aard en oorsprong als een cloud-gebaseerde scripttaal, waar traditionele console- of terminalgebaseerde uitvoeren (zoals stdout en stderr) minder relevant zijn. Historisch gezien was Google Apps Script ontworpen om de functionaliteit van Google Apps te verbeteren met eenvoudige scripts, met een focus op gebruiksgemak in plaats van de uitgebreide functies beschikbaar in meer complexe programmeeromgevingen.

Dat gezegd hebbende, de evolutie van Google Apps Script richting meer geavanceerde applicatieontwikkeling heeft ontwikkelaars ertoe aangezet creatieve benaderingen voor foutafhandeling en logging te adopteren, met gebruikmaking van beschikbare diensten zoals Logger en integratie met Google Cloud’s Operations Suite. Deze methoden, hoewel geen directe stderr-implementaties, bieden robuuste alternatieven voor foutbeheer en diagnostische logging in een cloudgerichte omgeving.

Van cruciaal belang is dat, hoewel deze methoden binnen het ecosysteem van Google Apps Script hun doel dienen, ze de beperkingen van het platform benadrukken in vergelijking met traditionele programmeeromgevingen. Voor ontwikkelaars die gedetailleerde en hiërarchische foutafhandelingsstrategieën nodig hebben, is integratie met externe loggingservices of het gebruik van Google Cloud Functions, die een meer conventionele stderr- en stdout-behandeling bieden, wellicht te verkiezen.
