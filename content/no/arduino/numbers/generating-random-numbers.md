---
title:                "Generering av tilfeldige tall"
aliases: - /no/arduino/generating-random-numbers.md
date:                  2024-01-27T20:32:47.071157-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generering av tilfeldige tall"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å generere tilfeldige tall i Arduino-prosjekter innebærer å produsere verdier som er uforutsigbare av design, noe som er avgjørende for applikasjoner som spill, simuleringer og sikkerhetssystemer. Programmerere bruker denne teknikken for å introdusere variabilitet eller ta beslutninger som ikke bør være deterministiske.

## Hvordan:
Arduino tilbyr enkle funksjoner for å generere tilfeldige tall: `randomSeed()` og `random()`. For å starte, så den tilfeldige nummergeneratoren for å sikre forskjellige sekvenser av tall hver gang programmet ditt kjører. En ofte brukt tilnærming er å så med en analog avlesning fra en ukoblet pin.

```Arduino
void setup() {
  Serial.begin(9600);
  // Initialiser tilfeldig seed
  randomSeed(analogRead(0));
}

void loop() {
  // Generer et tilfeldig tall mellom 0 og 99
  int randomNumber = random(100);
  Serial.println(randomNumber);
  delay(1000); // Forsinkelse på et sekund for lesbarhet av output
}
```

Programmet ovenfor initialiserer den tilfeldige nummergeneratoren i `setup()`-funksjonen og genererer et nytt tall mellom 0 og 99 i hver løkkeiterasjon, og skriver ut tallet til Serial Monitor.

Eksempel på output:
```
42
17
93
...
```

## Dypdykk
Arduinos `random()`-funksjon benytter seg under panseret av en pseudo-tilfeldig tallgenerator (PRNG), som følger en deterministisk sekvens, men ser statistisk tilfeldig ut. Den innledende verdien, eller såkornet, til sekvensen påvirker i høy grad dens uforutsigbarhet, derfor den vanlige bruken av `randomSeed()` med en noenlunde tilfeldig inngang som et utgangspunkt. Det er viktig å merke seg at tilfeldigheten som genereres av Arduino er tilstrekkelig for de fleste hobbyprosjekter, men kan ikke oppfylle kriteriene for høy-sikkerhetsapplikasjoner på grunn av dens forutsigbarhet over tid. For kryptografiske formål er det rådelig å se nærmere på mer sofistikerte algoritmer og maskinvaretilfeldige nummergeneratorer (HRNGs), som kan tilby ekte tilfeldighet ved å utnytte fysiske prosesser.
