---
aliases:
- /no/google-apps-script/refactoring/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:50.655600-07:00
description: "Omstrukturering i programmeringsspr\xE5ket refererer til prosessen med\
  \ \xE5 restrukturere eksisterende dataprogramkode - endre faktoriseringen uten \xE5\
  \ endre den\u2026"
lastmod: 2024-02-18 23:08:53.490257
model: gpt-4-0125-preview
summary: "Omstrukturering i programmeringsspr\xE5ket refererer til prosessen med \xE5\
  \ restrukturere eksisterende dataprogramkode - endre faktoriseringen uten \xE5 endre\
  \ den\u2026"
title: Refaktorering
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Omstrukturering i programmeringsspråket refererer til prosessen med å restrukturere eksisterende dataprogramkode - endre faktoriseringen uten å endre den eksterne oppførselen - for å forbedre ikke-funksjonelle attributter. Det er et viktig steg for programmerere å forbedre kodelesbarhet, redusere kompleksitet, og potensielt avdekke latente feil, noe som fremmer lettere vedlikehold og fremtidig kode-skalerbarhet.

## Hvordan gjøre det:

I Google Apps Script er et vanlig scenario som drar nytte av omstrukturering, forenklingen av tungvinte skript som samhandler med Google Sheets eller Docs. Opprinnelig kan skript være skrevet på en rask og skitten måte for å få resultater raskt. Over tid, ettersom skriptet vokser, blir det uhåndterlig. La oss gå gjennom et eksempel på omstrukturering for bedre lesbarhet og effektivitet.

**Opprinnelig skript:**

```javascript
function logSheetNames() {
  var sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  for (var i = 0; i < sheets.length; i++) {
    Logger.log(sheets[i].getName());
  }
}
```

Denne funksjonen logger navnet på hvert ark i et Google-regneark. Selv om det fungerer fint, bruker det utdaterte JavaScript-praksiser og mangler klarhet.

**Omstrukturert skript:**

```javascript
function logSheetNames() {
  const sheets = SpreadsheetApp.getActiveSpreadsheet().getSheets();
  sheets.forEach(sheet => Logger.log(sheet.getName()));
}
```

I den omstrukturerte versjonen har vi byttet til å bruke `const` for variabler som ikke endres, noe som gjør vår intensjon klarere. Vi har også brukt `forEach`-metoden, en mer moderne og kortfattet tilnærming for å iterere over matriser, noe som forbedrer lesbarheten.

**Eksempelutdata (for begge skriptene):**

Utdata i Logger vil se omtrent slik ut, forutsatt at Google Sheets-dokumentet ditt har to ark navngitt "Expenses" og "Revenue":

```
[20-04-2023 10:00:00: INFO] Expenses
[20-04-2023 10:00:01: INFO] Revenue
```

Det omstrukturerte skriptet oppnår det samme resultatet, men er renere og lettere å forstå ved første øyekast.

## Dypdykk

Omstrukturering i Google Apps Script arver delvis sine prinsipper fra den bredere programvareteknikkpraksisen. Det ble mer anerkjent og strukturert som et konsept på slutten av 1990-tallet, særlig på grunn av Martin Fowlers banebrytende bok "Refactoring: Improving the Design of Existing Code" (1999), som ga en omfattende guide til forskjellige omstruktureringsteknikker. Mens detaljene i omstrukturering kan variere på tvers av programmeringsspråk på grunn av deres syntaktiske og funksjonelle forskjeller, forblir det sentrale målet det samme: å forbedre koden uten å endre dens eksterne oppførsel.

I konteksten av Google Apps Script, er en nøkkelaspekt å vurdere under omstrukturering, tjenestekvoter og begrensninger pålagt av Google. Effektivt omstrukturert kode leses ikke bare bedre, men kjører også raskere og mer pålitelig innenfor disse begrensningene. For eksempel, batchoperasjoner (`Range.setValues()` i stedet for å sette verdier en celle om gangen) kan betydelig redusere utførelsestid og kvoteforbruk.

Det er imidlertid viktig å merke seg at for visse komplekse prosjekter, kan Google Apps Script falle kort på grunn av disse begrensningene. I slike tilfeller kan det å se på alternativer som Google Cloud Functions eller Apps Script's nyere søsken, AppSheet, tilby bedre skalerbarhet og funksjonalitet.

Til slutt, mens omstrukturering er en kritisk ferdighet i vedlikehold og forbedring av Google Apps Script-prosjekter, er forståelsen av miljøets begrensninger og vurderingen av alternative løsninger like viktig for å levere effektiv, robust og vedlikeholdbar kode.
