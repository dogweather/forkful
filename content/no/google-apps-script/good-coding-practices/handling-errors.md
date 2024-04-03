---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:02.798996-07:00
description: "Hvordan: Google Apps Script, som er basert p\xE5 JavaScript, lar oss\
  \ bruke den tradisjonelle `try-catch`-setningen for feilh\xE5ndtering, sammen med\
  \ `finally`\u2026"
lastmod: '2024-03-13T22:44:40.323758-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script, som er basert p\xE5 JavaScript, lar oss bruke den tradisjonelle\
  \ `try-catch`-setningen for feilh\xE5ndtering, sammen med `finally` hvis opprydding\
  \ er n\xF8dvendig uavhengig av suksess eller feil."
title: "H\xE5ndtering av feil"
weight: 16
---

## Hvordan:
Google Apps Script, som er basert på JavaScript, lar oss bruke den tradisjonelle `try-catch`-setningen for feilhåndtering, sammen med `finally` hvis opprydding er nødvendig uavhengig av suksess eller feil.

```javascript
function myFunction() {
  try {
    // Kode som kan forårsake en feil
    var sheet = SpreadsheetApp.getActiveSheet();
    var data = sheet.getRange("A1").getValue();
    if (data === "") {
      throw new Error("Celle A1 er tom.");
    }
    Logger.log(data);
  } catch (e) {
    // Kode for håndtering av feil
    Logger.log("Feil: " + e.message);
  } finally {
    // Oppryddingskode, utført enten en feil oppstod eller ikke
    Logger.log("Funksjon fullført.");
  }
}
```

Eksempel på utdata uten feil:
```
[Cellens verdi]
Funksjon fullført.
```

Eksempel på utdata med en feil (antatt at A1 er tom):
```
Feil: Celle A1 er tom.
Funksjon fullført.
```

Google Apps Script støtter også å kaste egendefinerte feil med `Error`-objektet og å fange spesifikke feiltyper etter behov. Imidlertid gjør mangel på avansert feilkategorisering det nødvendig å stole på feilmeldinger for spesifisitet.

## Dypdykk
Historisk sett har feilhåndtering i skriptspråk som JavaScript (og dermed Google Apps Script) vært mindre sofistikert enn i noen kompilerte språk, som tilbyr egenskaper som detaljerte unntakshierarkier og omfattende feilsøkingsverktøy. Google Apps Scripts modell er relativt grei, ved å utnytte JavaScripts `try-catch-finally`-paradigme. Denne enkelheten er i tråd med språkets design for å raskt utvikle og distribuere applikasjoner i liten til middels skala innenfor Googles økosystem, men det kan noen ganger begrense utviklere som håndterer komplekse feilscenarier.

I mer komplekse applikasjoner supplementerer programmerere ofte Google Apps Scripts innebygde feilhåndtering med egendefinerte logger og feilrapporteringsmekanismer. Dette kan inkludere å skrive feil til et Google-regneark for revisjon eller å bruke tredjeparts logger-tjenester gjennom Google Apps Scripts URL Fetch Services for å sende feildetaljer ut av skriptmiljøet.

Selv om Google Apps Script kanskje henger etter språk som Java eller C# når det gjelder innebygd kompleksitet og evner i feilhåndtering, gjør integrasjonen med Google-tjenester og enkelheten i `try-catch-finally`-tilnærmingen det til et kraftig verktøy for utviklere for å raskt automatisere oppgaver og skape integrasjoner innenfor Googles økosystem. Utviklere fra andre bakgrunner kan finne at utfordringen ikke ligger i å mestre komplekse feilhåndteringsmønstre, men i å kreativt utnytte det som er tilgjengelig for å sikre at deres skript er robuste og brukervennlige.
