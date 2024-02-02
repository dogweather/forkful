---
title:                "Bruke assosiative tabeller"
date:                  2024-02-01T22:04:18.663470-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke assosiative tabeller"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/google-apps-script/using-associative-arrays.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Assosiative tabeller, kjent som objekter i Google Apps Script (en variant av JavaScript), lar programmerere lage samlinger av nøkkel-verdi-par. Denne funksjonaliteten er avgjørende for å lagre og manipulere data effektivt, spesielt når man jobber med dynamisk navngitte egenskaper eller når en tradisjonell tabels lineære lagrings- og tilgangsmodell er utilstrekkelig.

## Slik gjør du:

I Google Apps Script oppretter og manipulerer du assosiative tabeller (objekter) ved hjelp av krøllparenteser `{}`, og definerer nøkkel-verdi-par innenfor. Nøkler er unike identifikatorer, og verdier kan være alt fra strenger og tall til mer komplekse objekter eller funksjoner. Her er et grunnleggende eksempel:

```javascript
function createAssociativeArray() {
  var bruker = {
    navn: "Ola Nordmann",
    alder: 30,
    epost: "ola.nordmann@eksempel.com"
  };

  // Tilgang til verdier
  Logger.log(bruker.navn); // Utdata: Ola Nordmann
  Logger.log(bruker["epost"]); // Utdata: ola.nordmann@eksempel.com

  // Legge til nye nøkkel-verdi-par
  bruker.tittel = "Programvareutvikler";
  bruker["land"] = "Norge";

  Logger.log(bruker.tittel); // Utdata: Programvareutvikler

  // Iterere over nøkkel-verdi-par
  for (var nøkkel in bruker) {
    Logger.log(nøkkel + ': ' + bruker[nøkkel]);
  }
}
```

Eksempelutdata for iterasjonsdelen kan se slik ut:
```
navn: Ola Nordmann
alder: 30
epost: ola.nordmann@eksempel.com
tittel: Programvareutvikler
land: Norge
```

Legg merke til hvordan du kan bruke både punktnotasjon og hakkeparentesnotasjon for å få tilgang til og sette egenskaper. Hakkeparentesnotasjon er spesielt nyttig når man jobber med nøkler som er dynamisk bestemt eller inkluderer tegn som ikke er tillatt i identifikatorer.

## Dypdykk

Assosiativ tabeller i form av objekter har vært en hjørnestein i JavaScript, og ved utvidelse Google Apps Script, som reflekterer dets prototypebaserte arvemekanisme. I motsetning til språk med tradisjonelle assosiative tabeller eller ordbøker (f.eks., Pythons dict), tilbyr Google Apps Script-objekter et fleksibelt og kraftig middel for å strukturere data, med fordel av JavaScripts dynamiske natur.

Det er imidlertid viktig å merke seg at ECMAScript 2015-spesifikasjonen introduserte `Map` og `Set` objekter, som tilbyr en mer grei håndtering av assosiativ samling med visse fordeler over objekter, som å vedlikeholde innsettingsrekkefølge og bedre ytelse for store datasett. Selv om Google Apps Script også støtter disse, avhenger valget mellom å bruke objekter eller de nyere `Map`/`Set`-strukturene av spesifikke behov og ytelseshensyn. For de fleste oppgaver med assosiative tabeller, gir tradisjonell objektbasert implementering en kjent og allsidig tilnærming, men det er tilrådelig å undersøke nyere alternativer ettersom kompleksiteten i skriptet ditt øker.
