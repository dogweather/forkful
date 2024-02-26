---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:55:02.798996-07:00
description: "Feilh\xE5ndtering i Google Apps Script handler om \xE5 forutse, fange\
  \ opp, og svare p\xE5 unntak eller feil som oppst\xE5r under kj\xF8ring av skriptet.\
  \ Programmerere\u2026"
lastmod: '2024-02-25T18:49:38.553362-07:00'
model: gpt-4-0125-preview
summary: "Feilh\xE5ndtering i Google Apps Script handler om \xE5 forutse, fange opp,\
  \ og svare p\xE5 unntak eller feil som oppst\xE5r under kj\xF8ring av skriptet.\
  \ Programmerere\u2026"
title: "H\xE5ndtering av feil"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Feilhåndtering i Google Apps Script handler om å forutse, fange opp, og svare på unntak eller feil som oppstår under kjøring av skriptet. Programmerere implementerer det for å beskytte skriptene mot uventede feil, slik at de sikrer jevnere, brukervennlige applikasjoner som kan håndtere eller logge feil grasiøst uten bråkrasj.

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
