---
title:    "Arduino: Lage tilfeldige tall"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Hvorfor

Å generere tilfeldige tall er en viktig del av koding i Arduino. Dette kan være nyttig for å lage spill, simuleringer og til og med sikkerhetsprotokoller. Å kunne generere tilfeldige tall kan også hjelpe utviklere med å teste kode og finne feil.

# Hvordan

For å generere tilfeldige tall i Arduino brukes funksjonen "random(min, max)" som returnerer et tilfeldig tall mellom minimum og maksimumsverdiene gitt. For å bruke denne funksjonen, må du inkludere den innebygde random biblioteket ved å legge følgende linje øverst i koden:

```Arduino
#include <random>
```

Deretter må du definere minimums- og maksimumsverdiene ved å skrive følgende linjer:

```Arduino
int min = 0; // minimumsverdi
int max = 100; // maksimumsverdi
```

For å generere og skrive ut et tilfeldig tall, bruker du følgende kode:

```Arduino
int randomNumber = random(min, max); // generer et tilfeldig tall
Serial.println(randomNumber); // skriver ut det tilfeldige tallet
```

Dette vil skrive ut et tilfeldig tall mellom 0 og 100 i serieovervåkingsvinduet.

# Deep Dive

For å virkelig forstå hvordan tilfeldige tall genereres i Arduino, må vi se på algoritmen som brukes. I utgangspunktet genereres det tilfeldige tallet ved å bruke en matematisk formel som involverer klokkeslettet og en tidligere generert tallverdi. Dette tallet kalles også et "seed" og er det som gjør at tallet blir ekte tilfeldig.

En viktig ting å huske på er at denne metoden for tilfeldig tallgenerering ikke er 100% nøyaktig, og det er mulig å få gjentatte tall i en lengre rekke. Derfor bør man alltid sørge for å endre "seed" hver gang man genererer et nytt tall for å få bedre tilfeldighet.

# Se også

Her er noen nyttige ressurser for å lære mer om tilfeldige tallgenerering i Arduino:

- [Offisiell Arduino dokumentasjon om random funksjonen](https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
- [Tutorial om å generere tilfeldige tall i Arduino](https://www.arduino.cc/en/tutorial/random-numbers)
- [Artikkel om tilfeldighet i koding](https://www.notion.so/Konsepter-i-koding-Episode-2-The-One-with-Randomness-a8d7dadd78da462fb6aea1966c352641) (på engelsk)