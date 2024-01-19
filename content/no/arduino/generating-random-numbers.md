---
title:                "Generere tilfeldige tall"
html_title:           "Arduino: Generere tilfeldige tall"
simple_title:         "Generere tilfeldige tall"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å generere tilfeldige tall er en prosess der datamaskiner produserer usystematiske tall. Dette er avgjørende for programmerere fordi det gir uforutsigbarhet - nødvendig i testing, simuleringer og generelle Sikkerhetsegenskaper som kryptering.

## Hvordan:

Er vi klare? La oss tilfeldigvis lage noen tall!

```Arduino
long randNumber;

void setup() {
  Serial.begin(9600);
  
  // initialize the random number generator:
  randomSeed(analogRead(0));
}

void loop() {
  // generate random number between 1 and 100:
  randNumber = random(1, 101);
  Serial.println(randNumber);

  delay(1000);  // Wait for 1 second
}
```
Eksempel utdata fra ovenfor:
```Arduino
23
67
39
89
...
```

## Dypdykk
Historisk sett, har vi brukt fysiske prosesser, som støy på en motstand, til å produsere tilfeldige tall. Men med moderne teknologi kan vi gjøre det programmatisk.

Alternativene til `random()` i Arduino inkluderer bruk av biblioteker som `TrueRandom` eller `Entropy`. Disse tilbyr mer komplekse og sikre måter å generere tilfeldige tall på.

Når det gjelder gjennomføringsdetaljer, bruker Arduino en pseudotilfeldig tallgenerator. Størrelsen og kvaliteten på tallene den genererer avhenger av innledende frøverdi (`randomSeed`) som er basert på en lest analog verdi (som ofte er støy og dermed tilfeldig).

## Se også

1. Arduino Reference: Random Numbers (https://www.arduino.cc/reference/en/language/functions/random-numbers/random/)
2. Arduino Reference: RandomSeed (https://www.arduino.cc/reference/en/language/functions/random-numbers/randomseed/)
3. Wikipedia: Random number generation (https://en.wikipedia.org/wiki/Random_number_generation)