---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:55.899784-07:00
description: "Afronden van getallen, een fundamenteel concept in computerprogrammering,\
  \ betreft het aanpassen van een getal naar het dichtstbijzijnde gehele getal of\u2026"
lastmod: '2024-02-25T18:49:47.722099-07:00'
model: gpt-4-0125-preview
summary: "Afronden van getallen, een fundamenteel concept in computerprogrammering,\
  \ betreft het aanpassen van een getal naar het dichtstbijzijnde gehele getal of\u2026"
title: Afronden van getallen
---

{{< edit_this_page >}}

## Wat & Waarom?

Afronden van getallen, een fundamenteel concept in computerprogrammering, betreft het aanpassen van een getal naar het dichtstbijzijnde gehele getal of naar een gespecificeerd aantal decimalen. Programmeurs voeren vaak afrondingen uit om getallen te vereenvoudigen voor de leesbaarheid voor mensen of om aan specifieke berekeningsbehoeften te voldoen, waarmee precisie verzekerd wordt en de rekenlast vermindert.

## Hoe:

Google Apps Script, als een op JavaScript gebaseerde taal, biedt standaardmethodes om getallen af te ronden. Hier volgt een uitleg van drie veelgebruikte technieken:

### Math.round()
Deze functie rondt een getal af naar het dichtstbijzijnde gehele getal.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Geeft uit: 3
```

### Math.ceil()
Rondt een getal af naar boven naar het dichtstbijzijnde gehele getal.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Geeft uit: 3
```

### Math.floor()
Rondt daarentegen een getal af naar beneden naar het dichtstbijzijnde gehele getal.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Geeft uit: 2
```

Voor specifieke decimalen, kun je `.toFixed()` gebruiken, wat eigenlijk een string teruggeeft, of een meer genuanceerde benadering voor wiskundige afronding:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Geeft uit: "2.57" (als een string)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Geeft uit: 2.57
```

## Diepgaand

Afronden van getallen in Google Apps Script wijkt niet veel af van hoe het wordt gedaan in andere JavaScript-omgevingen. Het begrijpen van de verschillen in afrondingsmethoden en het potentieel voor problemen met drijvende-kommagetallen is echter cruciaal. Bijvoorbeeld, vanwege de manier waarop computers floats voorstellen, kunnen niet alle decimale breuken met perfecte nauwkeurigheid worden weergegeven, wat soms tot onverwachte afrondingsresultaten leidt.

Historisch gezien gaat JavaScript (en bij uitbreiding Google Apps Script) hiermee om door zich te conformeren aan de IEEE 754-standaard, gebruikt door veel andere programmeertalen voor berekeningen met drijvende komma. Deze standaard definieert hoe getallen worden afgerond, waardoor consistentie over verschillende platforms en talen heen wordt verzekerd.

Hoewel directe afrondingsmethoden in Google Apps Script eenvoudig en vaak voldoende zijn, kunnen complexe of precisie-gerichte toepassingen baat hebben bij bibliotheken zoals decimal.js of big.js, die zijn ontworpen om berekeningen met willekeurige precisie te verwerken. Deze kunnen vooral nuttig zijn bij financiële of wetenschappelijke berekeningen waarbij de nauwkeurigheid van afgeronde getallen van het grootste belang is.

Onthoud echter dat het gebruiken van externe bibliotheken in Google Apps Script vereist dat ze worden geladen via de script-editor, wat afhankelijkheden kan introduceren of de prestaties van je script kan beïnvloeden afhankelijk van hoe het wordt gebruikt. In veel gevallen zijn de ingebouwde Math-methoden volledig toereikend, maar voor die randgevallen die precisie tot op de n-de graad vereisen, kan het nodig zijn om verder te kijken dan de standaardbibliotheek.
