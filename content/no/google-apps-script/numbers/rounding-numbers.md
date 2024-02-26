---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:00:42.422121-07:00
description: "Avrunding av tall, et grunnleggende konsept innen dataprogrammering,\
  \ inneb\xE6rer \xE5 justere et tall til n\xE6rmeste heltall eller til et spesifisert\
  \ antall\u2026"
lastmod: '2024-02-25T18:49:38.539374-07:00'
model: gpt-4-0125-preview
summary: "Avrunding av tall, et grunnleggende konsept innen dataprogrammering, inneb\xE6\
  rer \xE5 justere et tall til n\xE6rmeste heltall eller til et spesifisert antall\u2026"
title: Avrunding av tall
---

{{< edit_this_page >}}

## Hva og hvorfor?

Avrunding av tall, et grunnleggende konsept innen dataprogrammering, innebærer å justere et tall til nærmeste heltall eller til et spesifisert antall desimaler. Programmerere utfører ofte avrunding for å forenkle tall for menneskelig lesbarhet eller for å møte spesifikke beregningsbehov, noe som sikrer presisjon og reduserer beregningsbelastningen.

## Hvordan:

Google Apps Script, som er et JavaScript-basert språk, tilbyr standardmetoder for å avrunde tall. Her er en oppdeling av tre ofte brukte teknikker:

### Math.round()
Denne funksjonen avrunder et tall til nærmeste heltall.

```javascript
var number = 2.56;
var roundedNumber = Math.round(number); 
Logger.log(roundedNumber); // Utganger: 3
```

### Math.ceil()
Avrunder et tall opp til nærmeste heltall.

```javascript
var number = 2.56;
var roundedUp = Math.ceil(number); 
Logger.log(roundedUp); // Utganger: 3
```

### Math.floor()
I motsetning, avrunder et tall ned til nærmeste heltall.

```javascript
var number = 2.56;
var roundedDown = Math.floor(number); 
Logger.log(roundedDown); // Utganger: 2
```

For spesifikke desimalplasser kan du bruke `.toFixed()`, som faktisk returnerer en streng, eller en mer nyansert tilnærming for matematisk avrunding:

```javascript
var number = 2.56789;
var fixedNumber = number.toFixed(2); 
Logger.log(fixedNumber); // Utganger: "2.57" (som en streng)

var preciseRound = Math.round(number * 100) / 100; 
Logger.log(preciseRound); // Utganger: 2.57
```

## Dypdykk

Å avrunde tall i Google Apps Script avviker ikke mye fra hvordan det gjøres i andre JavaScript-miljøer. Men det er avgjørende å forstå forskjellene i avrundingsmetoder og potensialet for problemer med flyttallsaritmetikk. For eksempel, på grunn av måten datamaskiner representerer flyttall på, kan ikke alle desimalfraksjoner representeres med perfekt nøyaktighet, noe som fører til noen ganger uventede avrundingsresultater.

Historisk sett håndterer JavaScript (og ved forlengelse, Google Apps Script) dette ved å følge IEEE 754-standarden, som brukes av mange andre programmeringsspråk for flyttallsaritmetikk. Denne standarden definerer hvordan tall blir avrundet, og sikrer konsistens på tvers av forskjellige plattformer og språk.

Selv om direkte avrundingsmetoder i Google Apps Script er greie og ofte tilstrekkelige, kan komplekse eller høy-presisjonsapplikasjoner ha nytte av biblioteker som decimal.js eller big.js, som er designet for å håndtere vilkårlig presisjonsaritmetikk. Disse kan være spesielt nyttige når du jobber med finansielle eller vitenskapelige beregninger der nøyaktigheten av avrundede tall er av ytterste viktighet.

Husk, selv om å benytte eksterne biblioteker i Google Apps Script krever at du laster dem gjennom skripteditoren, noe som kan introdusere avhengigheter eller påvirke ytelsen til skriptet ditt avhengig av hvordan det brukes. I mange tilfeller er de innebygde Matematikk-metodene helt tilstrekkelige, men for de tilfellene som krever presisjon til n-te grad, kan det være nødvendig å se utover standardbiblioteket.
