---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:57.339456-07:00
description: "Hvordan: Selv om Google Apps Script ikke har et innebygd testrammeverk\
  \ som noen andre programmeringsmilj\xF8er, kan du fortsatt skrive og kj\xF8re tester\
  \ ved \xE5\u2026"
lastmod: '2024-03-13T22:44:40.317949-06:00'
model: gpt-4-0125-preview
summary: "Selv om Google Apps Script ikke har et innebygd testrammeverk som noen andre\
  \ programmeringsmilj\xF8er, kan du fortsatt skrive og kj\xF8re tester ved \xE5 utnytte\
  \ enkle GAS-funksjoner eller integrere eksterne testbiblioteker som `QUnit`."
title: Skrive tester
weight: 36
---

## Hvordan:
Selv om Google Apps Script ikke har et innebygd testrammeverk som noen andre programmeringsmiljøer, kan du fortsatt skrive og kjøre tester ved å utnytte enkle GAS-funksjoner eller integrere eksterne testbiblioteker som `QUnit`. Her er et grunnleggende eksempel som bruker en enkel GAS-funksjon for å teste en annen funksjon i skriptet ditt:

```javascript
function add(a, b) {
  return a + b;
}

function testAdd() {
  var resultat = add(2, 3);
  if (resultat !== 5) {
    throw new Error("Test mislyktes: add(2, 3) burde være 5, men var " + resultat);
  } else {
    Logger.log("Test bestått!");
  }
}
```

Å kjøre `testAdd()` vil logge "Test bestått!" hvis `add`-funksjonen fungerer korrekt, eller kaste en feil hvis den ikke gjør det. For en mer sofistikert tilnærming, innebærer integrering av QUnit med Google Apps Script noen flere trinn, men tilbyr et kraftig testmiljø. Et eksempel på QUnit-testoppsett ser slik ut:

1. Inkluder QUnit-biblioteket i prosjektet ditt.
2. Opprett en test HTML-fil for å kjøre QUnit-testene.
3. Skriv testtilfeller ved å bruke QUnits syntaks.

Her er et eksempel med QUnit:

```javascript
// Inkluder QUnit ved å lenke til det i en HTML-fil brukt for å kjøre testene dine

QUnit.test("Tester add funksjonen", function (assert) {
  var resultat = add(2, 3);
  assert.equal(resultat, 5, "add(2, 3) burde returnere 5");
});
```

For å se resultatene, åpne HTML-filen innenfor GAS Script Editor eller distribuer den som en webapp.

## Dypdykk
Historisk sett har testing i Google Apps Script blitt noe oversett, sannsynligvis på grunn av plattformens opprinnelse og primære bruksområder som fokuserer på raske, småskala automatiseringsoppgaver heller enn store applikasjoner. Som sådan, tilbyr ikke GAS de samme robuste testrammeverkene og verktøyene funnet i mer tradisjonelle programmeringsmiljøer. Men, fellesskapet har tilpasset seg ved å inkorporere åpen kildekode-biblioteker og kreativt utnytte Googles eksisterende verktøy.

Bruk av biblioteker som QUnit representerer et betydelig skritt fremover, men kommer med egne utfordringer, som å sette opp et passende testmiljø og lære en tilleggs syntaks. Imidlertid, for de som er investert i å bygge mer komplekse og pålitelige applikasjoner med GAS, er innsatsen verdt det.

Alternativer som å bruke enkle GAS-funksjoner for testing tilbyr enkel bruk og integrasjon med GAS-miljøet uten ekstra avhengigheter, men mangler omfattende testfunksjoner og evnen til enkelt å skalere når prosjektet ditt vokser. Verktøy som clasp (Google Apps Script Command Line Interface) kan legge til rette for mer avanserte arbeidsflyter, inkludert testing, ved å tillate utviklere å kode i deres foretrukne IDE, og introdusere rom for sømløs integrering med eksterne testrammeverk.

Til slutt, selv om GAS kanskje ikke har støtte for sofistikerte tester rett ut av boksen, gir fleksibiliteten og fellesskapets innovative tilnærminger levedyktige veier for å sikre at skriptene dine er robuste, pålitelige og klare for enhver oppgave.
