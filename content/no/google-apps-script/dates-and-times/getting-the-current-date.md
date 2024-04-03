---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:54:28.008327-07:00
description: "\xC5 f\xE5 tak i dagens dato i Google Apps Script handler om \xE5 hente\
  \ den levende datoen og tiden, en vanlig oppgave for automatisering, logging og\
  \ tidsstempling\u2026"
lastmod: '2024-03-13T22:44:40.327006-06:00'
model: gpt-4-0125-preview
summary: "\xC5 f\xE5 tak i dagens dato i Google Apps Script handler om \xE5 hente\
  \ den levende datoen og tiden, en vanlig oppgave for automatisering, logging og\
  \ tidsstempling i apper knyttet til Googles \xF8kosystem."
title: "F\xE5 den gjeldende datoen"
weight: 29
---

## Hva & Hvorfor?

Å få tak i dagens dato i Google Apps Script handler om å hente den levende datoen og tiden, en vanlig oppgave for automatisering, logging og tidsstempling i apper knyttet til Googles økosystem. Programmerere bruker dette til generering av dynamisk innhold, fristovervåking og planlegging innen Google Docs, Sheets og andre Google-tjenester.

## Hvordan:

Google Apps Script, som er basert på JavaScript, tilbyr enkle metoder for å få tak i dagens dato. Du kan bruke `new Date()`-konstruktøren for å lage et nytt datoombjekt som representerer dagens dato og tid. Her er hvordan du kan manipulere og vise dette i forskjellige formater.

```javascript
function visDagensDato() {
  var gjeldendeDato = new Date();
  
  Logger.log(gjeldendeDato); // Logger gjeldende dato og tid i scriptets tidssone
  
  // For å vise kun datoen i ÅÅÅÅ-MM-DD-format
  var datoStreng = gjeldendeDato.getFullYear() + '-' + 
                   (gjeldendeDato.getMonth() + 1).toString().padStart(2, '0') + '-' + 
                   gjeldendeDato.getDate().toString().padStart(2, '0');
  Logger.log(datoStreng); // Eksempeloppgave: "2023-04-01"
  
  // Visning i et mer leselig format
  var alternativer = { year: 'numeric', month: 'long', day: 'numeric', hour: '2-digit', minute: '2-digit', second: '2-digit', timeZoneName: 'short' };
  var lesbarDato = gjeldendeDato.toLocaleDateString('en-US', alternativer) + ' ' + 
                     gjeldendeDato.toLocaleTimeString('en-US', alternativer);
                     
  Logger.log(lesbarDato); // Eksempeloppgave: "April 1, 2023, 12:00:00 PM GMT+1"
}
```

Disse bitene demonstrerer hvordan man kan fange og formatere gjeldende dato og tid, og viser allsidigheten for forskjellige programmeringsbehov innen Google Apps Script.

## Dypdykk

Før JavaScript fastsatte `Date`-objektet, måtte programmerere manuelt holde styr på tid og dato gjennom mindre standardiserte og mer byrdefulle midler. Dette inkluderte bruk av tidsstempel heltall og hjemmelagde datofunksjoner, som varierte fra ett programmeringsmiljø til et annet, noe som førte til inkonsekvens og kompatibilitetsproblemer.

Introduksjonen av `new Date()`-objektet i JavaScript, og ved utvidelse Google Apps Script, standardiserte dato- og tidsoperasjoner, gjorde dem mer intuitive og reduserte mengden av kode nødvendig for dato-relaterte operasjoner. Det er verdt å merke seg at mens Google Apps Scripts implementering er praktisk og tilstrekkelig for mange applikasjoner innenfor Googles produktsuite, kan det hende at det ikke dekker alle scenarioer, spesielt de som krever kompleks tidssonehåndtering eller presis tidsstempellogging i raske miljøer.

For slike avanserte bruksområder, vender programmerere ofte til biblioteker som Moment.js eller date-fns i JavaScript. Selv om Google Apps Script ikke støtter disse bibliotekene i seg selv, kan utviklere etterligne noen av deres funksjonaliteter ved å bruke tilgjengelige JavaScript Date-metoder eller ved å få tilgang til eksterne biblioteker gjennom HTML Service eller Apps Scripts URL Fetch-tjeneste. Til tross for disse alternativene, forblir enkelheten og integreringen av Google Apps Scripts native dato- og tidsfunksjoner en go-to for de fleste oppgaver i Google-økosystemet.
