---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:07:52.650120-07:00
description: "Hur man g\xF6r: Att avrunda tal i C kan \xE5stadkommas med olika funktioner,\
  \ men det vanligaste tillv\xE4gag\xE5ngss\xE4ttet involverar funktionerna `floor()`,\
  \ `ceil()`,\u2026"
lastmod: '2024-03-13T22:44:38.376723-06:00'
model: gpt-4-0125-preview
summary: "Att avrunda tal i C kan \xE5stadkommas med olika funktioner, men det vanligaste\
  \ tillv\xE4gag\xE5ngss\xE4ttet involverar funktionerna `floor()`, `ceil()`, och\
  \ `round()`."
title: Avrundning av nummer
weight: 13
---

## Hur man gör:
Att avrunda tal i C kan åstadkommas med olika funktioner, men det vanligaste tillvägagångssättet involverar funktionerna `floor()`, `ceil()`, och `round()`. Dessa funktioner är en del av standardmatematikbiblioteket, så du måste inkludera `math.h` i ditt program.

```c
#include <stdio.h>
#include <math.h>

int main() {
    double num = 9.527;

    // Använder floor() för att avrunda nedåt
    double floorResult = floor(num);
    printf("floor(9.527) = %.0f\n", floorResult);

    // Använder ceil() för att avrunda uppåt
    double ceilResult = ceil(num);
    printf("ceil(9.527) = %.0f\n", ceilResult);

    // Använder round() för att avrunda till närmaste heltal
    double roundResult = round(num);
    printf("round(9.527) = %.0f\n", roundResult);

    // Avrundning till ett specificerat antal decimaler innebär multiplikation och division
    double twoDecimalPlaces = round(num * 100) / 100;
    printf("Avrundning till två decimaler: %.2f\n", twoDecimalPlaces);

    return 0;
}
```

Utdata:
```
floor(9.527) = 9
ceil(9.527) = 10
round(9.527) = 10
Avrundning till två decimaler: 9.53
```

## Fördjupning
Avrundning av tal har djupa historiska rötter i matematik och beräkning, oerhört viktigt för både teoretiska och tillämpade aspekter. I C ger `floor()`, `ceil()`, och `round()` grundläggande funktionalitet, men essensen av att avrunda flytpunkttal till heltal eller specifika decimalställen är mer nyanserad på grund av den binära representationen av flytpunkttal. Denna representation kan leda till oväntade resultat på grund av hur tal som inte kan representeras exakt i binärt (som 0.1) hanteras.

Dessa funktioner är en del av C-standardbiblioteket, definierat i `<math.h>`. När du avrundar tal, särskilt för finansiella eller precisa ingenjörsberäkningar, måste man överväga implikationerna av att använda binära flytpunkttal. Alternativ till de inbyggda C-funktionerna för mycket exakt eller decimal-specifik avrundning kan inkludera att implementera anpassade avrundningsfunktioner eller att använda bibliotek designade för godtycklig precision i aritmetiken, som GMP eller MPFR, även om dessa introducerar ytterligare komplexitet och beroenden.

I praktiken innebär valet av rätt tillvägagångssätt för avrundning i C att balansera behovet av precision, prestanda och praktiska överväganden, med en skarp förståelse för de domänspecifika kraven för applikationen som utvecklas.
