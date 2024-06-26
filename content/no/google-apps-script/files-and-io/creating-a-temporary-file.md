---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:51:51.843648-07:00
description: "Hvordan: I Google Apps Script kan du lage en midlertidig fil ved \xE5\
  \ bruke DriveApp-tjenesten, som gir en enkel metode for \xE5 opprette, lese og slette\
  \ filer\u2026"
lastmod: '2024-03-13T22:44:40.337143-06:00'
model: gpt-4-0125-preview
summary: "I Google Apps Script kan du lage en midlertidig fil ved \xE5 bruke DriveApp-tjenesten,\
  \ som gir en enkel metode for \xE5 opprette, lese og slette filer i Google Drive."
title: Opprette en midlertidig fil
weight: 21
---

## Hvordan:
I Google Apps Script kan du lage en midlertidig fil ved å bruke DriveApp-tjenesten, som gir en enkel metode for å opprette, lese og slette filer i Google Drive. Her er hvordan du kan lage en midlertidig tekstfil, skrive litt data til den, og deretter fjerne den etter bruk:

```javascript
function createTemporaryFile() {
  // Opprett en midlertidig fil med navnet "tempFile.txt"
  var tempFile = DriveApp.createFile('tempFile.txt', 'Midlertidig innhold', MimeType.PLAIN_TEXT);
  
  // Logg filens URL for tilgang eller feilsøking
  Logger.log('Midlertidig fil opprettet: ' + tempFile.getUrl());
  
  // Eksempel på operasjon: Lese filinnholdet
  var innhold = tempFile.getBlob().getDataAsString();
  Logger.log('Innholdet i tempFile: ' + innhold);
  
  // Forutsatt at operasjonen er fullført og filen ikke lenger er nødvendig
  // Fjern den midlertidige filen
  tempFile.setTrashed(true);
  
  // Bekreft sletting
  Logger.log('Midlertidig fil slettet');
}
```

Å kjøre dette skriptet vil utgi:

```
Midlertidig fil opprettet: [URL til den opprettede midlertidige filen]
Innholdet i tempFile: Midlertidig innhold
Midlertidig fil slettet
```

Dette eksempelskriptet viser opprettingen av en midlertidig fil, utfører en operasjon for å lese innholdet, og fjerner til slutt filen for å rydde opp.

## Dypdykk
Konseptet med å lage midlertidige filer i programvareutvikling er like gammelt som konseptet med filbehandling selv. I tradisjonelle filsystemer blir midlertidige filer ofte opprettet i utpekte temp-kataloger og er avgjørende for forskjellige mellomliggende prosesser, som sortering av store datasett, holding av sesjonsdata for webapplikasjoner, eller lagring av datasegmenter under filkonverteringsprosesser.

I Google Apps Script utnytter prosessen med å lage midlertidige filer Google Drives infrastruktur, som tilbyr en interessant blanding av skybasert filbehandling med tradisjonelle programmeringskonsepter. Men denne metoden for å lage midlertidige filer i Google Drive er ikke uten begrensninger og kostnader, med tanke på kvotegrensene Google Drive pålegger. Også latenstiden ved å få tilgang til Google Drive over nettverket sammenlignet med et lokalt filsystem kan være en kritisk faktor for applikasjoner med høy ytelse.

Som alternativer kan utviklere vurdere å bruke Google Sheets for små datasett som krever midlertidig lagring under beregning, eller Google Cloud Storage for applikasjoner som krever høyytelses lese-/skriveoperasjoner og større lagringskapasiteter. Hver av disse løsningene tilbyr forskjellige kompromisser med hensyn til latenstid, lagringsgrenser og brukervennlighet fra Google Apps Script. Til slutt avhenger valget av de spesifikke kravene til applikasjonen og den eksisterende infrastrukturen den opererer innenfor.
