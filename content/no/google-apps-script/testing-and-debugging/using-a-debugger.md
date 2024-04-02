---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:03:17.586174-07:00
description: "Google Apps Script tilbyr en innebygd feils\xF8ker i Apps Script Editor\
  \ for \xE5 hjelpe med \xE5 feils\xF8ke skript. Her er hvordan du starter og bruker\
  \ feils\xF8keren:\u2026"
lastmod: '2024-03-13T22:44:40.320603-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script tilbyr en innebygd feils\xF8ker i Apps Script Editor\
  \ for \xE5 hjelpe med \xE5 feils\xF8ke skript. Her er hvordan du starter og bruker\
  \ feils\xF8keren:\u2026"
title: Bruker en debugger
weight: 35
---

## Hvordan:
Google Apps Script tilbyr en innebygd feilsøker i Apps Script Editor for å hjelpe med å feilsøke skript. Her er hvordan du starter og bruker feilsøkeren:

1. **Åpne skriptet ditt i Apps Script Editor.**
2. **Velg en funksjon å feilsøke.** Fra rullegardinmenyen på toppen, velg funksjonen du ønsker å feilsøke.
3. **Sett brytepunkter.** Klikk på margen (det grå området til venstre for linjenumrene) hvor du ønsker å pause utførelsen; et rødt punkt dukker opp, som indikerer et brytepunkt.
4. **Start feilsøkingen.** Klikk på feilrettingsikonet eller velg `Feilsøk` > `Start feilsøking`. Utførelsen vil starte og pause ved det første brytepunktet.

Vurder dette enkle skriptet:

```javascript
function calculateSum() {
  var a = 5;
  var b = 10;
  var sum = a + b;
  Logger.log(sum); // Har til hensikt å logge 15
}
```

Hvis du er usikker på hvorfor `Logger.log(sum)` ikke viser det forventede resultatet, kunne du sette et brytepunkt ved linjen `var sum = a + b;` og gå gjennom skriptet linje for linje for å inspisere variabelverdier.

**Eksempelutskrift i Logger:**

```plain
15
```

Mens du feilsøker, tillater Apps Script Editor deg å:

- **Gå gjennom koden** ved bruk av steg over, steg inn, og steg ut-knappene.
- **Se på uttrykk og variabler** for å se verdiene deres endre seg i sanntid.
- **Inspeksjon av kallstakken** for å spore funksjonskall.

## Dypdykk
Feilsøking i Google Apps Script, som i ethvert annet programmeringsmiljø, er avgjørende for å skape feilfrie applikasjoner. Introdsuert tidlig i utviklingen av GAS, tilbyr den innebygde feilsøkeren grunnleggende evner til å inspisere og rette koden inkrementelt. Selv om den tilbyr grunnleggende feilsøkingsfunksjoner lik de som finnes i mer modne miljøer som Visual Studio Code eller IntelliJ, kan den være begrensende for komplekse feilsøkningsscenarioer. For eksempel, dens evner til å inspisere asynkrone tilbakeslag eller håndtere tunge skriptutførelser kunne være begrensende.

For komplekse feilsøkingsbehov, kan utviklere ty til alternative metoder som omfattende logging (ved bruk av `Logger.log()`) eller til og med distribuere som en webapplikasjon for å inspisere oppførselen i et virkelig scenario. Imidlertid gjør enkelheten og integreringen av GAS's feilsøker innen Apps Script Editor den til et uvurderlig første skritt for feilsøking og forståelse av skriptatferd. Merkbart med Googles kontinuerlige oppdateringer og forbedringer til Apps Script, forbedres feilsøkingserfaringen stadig, og tilbyr mer sofistikerte verktøy og alternativer over tid. Denne utviklingen reflekterer Googles forpliktelse til å gjøre Apps Script til en kraftigere og mer tilgjengelig plattform for utviklere fra ulike bakgrunner.
