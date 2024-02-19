---
aliases:
- /no/google-apps-script/calculating-a-date-in-the-future-or-past/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:49:07.588375-07:00
description: "\xC5 beregne en dato i fremtiden eller fortiden handler om \xE5 manipulere\
  \ datoomr\xE5der for \xE5 finne datoer utover eller f\xF8r den n\xE5v\xE6rende datoen,\
  \ henholdsvis.\u2026"
lastmod: 2024-02-18 23:08:53.495690
model: gpt-4-0125-preview
summary: "\xC5 beregne en dato i fremtiden eller fortiden handler om \xE5 manipulere\
  \ datoomr\xE5der for \xE5 finne datoer utover eller f\xF8r den n\xE5v\xE6rende datoen,\
  \ henholdsvis.\u2026"
title: Beregning av en dato i fremtiden eller fortiden
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å beregne en dato i fremtiden eller fortiden handler om å manipulere datoområder for å finne datoer utover eller før den nåværende datoen, henholdsvis. Programmerere gjør dette for oppgaver som spenner fra å sette påminnelser og utløpsdatoer til å analysere trender basert på tid.

## Hvordan:

I Google Apps Script, som er basert på JavaScript, kan du manipulere datoer ved hjelp av `Date`-objektet. Her er hvordan du kan beregne datoer i fremtiden og fortiden:

### Beregning av fremtidig dato

For å beregne en fremtidig dato, oppretter du et datoobjekt for dagens dato og deretter legger du til det ønskede antall dager (eller andre tidsenheter) til den.

```javascript
// Dagens dato
var today = new Date();

// Beregn en dato 10 dager inn i fremtiden
var futureDate = new Date(today);
futureDate.setDate(today.getDate() + 10);

Logger.log("Fremtidig dato: " + futureDate.toDateString());
```

### Beregning av tidligere dato

På samme måte, for å finne en dato i fortiden, trekker du fra antall dager fra dagens dato.

```javascript
// Dagens dato
var today = new Date();

// Beregn en dato 10 dager tilbake i tid
var pastDate = new Date(today);
pastDate.setDate(today.getDate() - 10);

Logger.log("Tidligere dato: " + pastDate.toDateString());
```

### Eksempel på utdata

Dette ville gi noe som følgende (med antakelse av at i dag er 15. april 2023):

```
Fremtidig dato: Tir Apr 25 2023
Tidligere dato: Ons Apr 05 2023
```

Husk, `Date`-objektet i JavaScript (og dermed i Google Apps Script) justerer automatisk måneder og år når du legger til eller trekker fra dager.

## Dypdykk

Manipulering av datoer ved hjelp av `Date`-objektet stammer fra tidlige JavaScript-implementeringer. Over tid har denne tilnærmingen generelt forblitt konsistent, og gir en grei måte for utviklere å håndtere datoer uten å trenge eksterne biblioteker. Imidlertid, for mer komplekse operasjoner som tidssonejusteringer, eller når man jobber med omfattende datobasert data, kan biblioteker som `Moment.js` eller den mer moderne `Luxon` tilby mer funksjonalitet og enklere håndtering.

I Google Apps Script, spesielt, til tross for direkte tilgjengelighet og enkelheten til `Date`-objektet, er det viktig å være oppmerksom på hvordan datoberegninger kan påvirke scriptets ytelse og kjøretid, spesielt i tidsdrevne utløsere eller omfattende regnearkmanipulasjoner. I tillegg, selv om Google Apps Script gir innebygde metoder for å håndtere datoer innenfor sitt økosystem (som i Google Sheets eller Kalender), kan integrering av eksterne biblioteker eller utnyttelse av Googles avanserte tjenester noen ganger gi mer robuste løsninger for komplekse scenarioer.

Så, mens den native JavaScript `Date`-objektmetodikken vanligvis er tilstrekkelig for enkle beregninger, kan utforsking av eksterne biblioteker eller tjenester forbedre funksjonaliteten for mer nyanserte krav.
