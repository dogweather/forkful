---
title:                "Generera slumpmässiga tal"
date:                  2024-01-20T17:48:43.206364-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generera slumpmässiga tal"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att generera slumptal innebär att skapa nummer som inte kan förutses logiskt. Programmerare använder slumptal för spel, simuleringsprogram och för att ge sina projekt en känsla av oförutsägbarhet.

## Så här gör du:

```Arduino
// Initiera en slumpmässig sekvens
void setup() {
  Serial.begin(9600);
  randomSeed(analogRead(0)); // Använd en oanvänd analog pin för att få en slumpmässig startpunkt
}

// Generator för ett slumpmässigt tal och skriv ut det
void loop() {
  int randomValue = random(0, 100); // Genererar ett tal mellan 0 och 99
  Serial.println(randomValue);
  delay(1000); // Vänta 1 sekund mellan varje generering
}
```
Sample output:
```
45
23
77
...
```

## Djupdykning:

I tidiga datorer var slumptalsgenerering inte lika sofistikerad; de använde ofta enkla matematiska formelbaserade algoritmer. Arduino använder en pseudoslumptalsgenerator (PRNG), vilket innebär att sekvensen av tal är förutbestämd och upprepas efter ett visst intervall. Funktionen `randomSeed()` är kritisk då den initierar PRNG med ett startvärde, vilket gör slumptalssekvensen mer oförutsägbar. Alternativt kan hårdvarubaserade generatorer användas för äkta slumptal, men de är mer komplexa och dyra. Implementationen i Arduino är tillräcklig för de flesta hobbyprojekt.

## Se även:

- Arduino `random` referens: https://www.arduino.cc/reference/en/language/functions/random-numbers/random/
- Tutorial på användning av random numbers i Arduino: https://create.arduino.cc/projecthub/harshmangukiya/random-number-generator-using-arduino-9a9b28
- Diskussion om pseudoslumptal vs äkta slumptal: http://www.arduino.cc/en/Tutorial/RandomSeed
