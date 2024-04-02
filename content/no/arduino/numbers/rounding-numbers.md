---
date: 2024-01-26 03:42:41.564841-07:00
description: "Avrunding av tall inneb\xE6rer \xE5 trimme et desimaltall til sin n\xE6\
  rmeste hele verdi eller til et bestemt antall desimalplasser. Programmerere runder\
  \ av tall\u2026"
lastmod: '2024-03-13T22:44:41.053603-06:00'
model: gpt-4-0125-preview
summary: "Avrunding av tall inneb\xE6rer \xE5 trimme et desimaltall til sin n\xE6\
  rmeste hele verdi eller til et bestemt antall desimalplasser. Programmerere runder\
  \ av tall\u2026"
title: Avrunding av tall
weight: 13
---

## Hva og hvorfor?
Avrunding av tall innebærer å trimme et desimaltall til sin nærmeste hele verdi eller til et bestemt antall desimalplasser. Programmerere runder av tall for å gjøre dem enklere å lese og håndtere, spesielt når presisjon utover et visst punkt er unødvendig eller kan føre til feil.

## Hvordan:
I Arduino kan du avrunde tall ved å bruke innebygde funksjoner. Viktige aktører er `round`, `ceil` og `floor`. Her er en rask demo:

```arduino
void setup() {
  Serial.begin(9600);
  
  float myNumber = 123.4567;

  // Avrunder til nærmeste hele tall
  Serial.println(round(myNumber)); // Utganger: 123

  // Avrunder alltid opp
  Serial.println(ceil(myNumber));  // Utganger: 124

  // Avrunder alltid ned
  Serial.println(floor(myNumber)); // Utganger: 123
}

void loop() {
  // Ingenting å loope gjennom.
}
```

## Dykking dypere:
Avrundingsalgoritmer har en lang historie; de har vært rundt lenge før digitale datamaskiner. I analog databehandling var avrunding en fysisk prosess. I digital databehandling er det en matematisk prosess.

Avrunding er nødvendig når vi konverterer fra en type med mer presisjon (som `float` eller `double`) til en type med mindre presisjon (som `int`). Men hvordan vi avrunder kan variere:

1. `round()`: Standard avrunding. Hvis fraksjonen er 0,5 eller høyere, går den opp; ellers går den ned.
2. `ceil()`: Kort for "tak", avrunder alltid opp til nærmeste hele tall, selv om det er nærmere det lavere tallet.
3. `floor()`: Motsatt av tak; avrunder alltid ned.

Valget mellom disse funksjonene avhenger av hva den avrundede verdien er for. Målinger kan trenge standard avrunding, penger bruker ofte `floor`, mens lagersystemer kan bruke `ceil` for å sikre at alt er tatt med.

Arduinos implementering av disse funksjonene er grei; de håndterer ikke ekstra tilfeller som avrunding til spesifikke desimalplasser. For det er en tilpasset funksjon eller dypere matematikk som kommer i spill - tenk på å multiplisere for å skifte desimalet, avrunde, og deretter dele tilbake.

Avrundingsfeil kan akkumuleres, noe som betydelig påvirker lange beregninger eller iterative prosesser. Programmerere må være forsiktige når de kjører mange operasjoner på avrundede verdier.

## Se også:
2. Dypere titt på fallgruvene og strategiene for avrunding: [Floating Point Guide](https://floating-point-gui.de/)
3. For avanserte teknikker, inkludert tilpassede avrundingsfunksjoner og håndtering av avrundingsfeil, kan du sjekke akademiske ressurser eller detaljerte programmeringsguider.
