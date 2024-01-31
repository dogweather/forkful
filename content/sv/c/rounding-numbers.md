---
title:                "Avrundning av tal"
date:                  2024-01-26T03:43:08.220500-07:00
model:                 gpt-4-0125-preview
simple_title:         "Avrundning av tal"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## Vad & varför?
Att avrunda tal innebär att man klipper av siffrorna efter en viss punkt och eventuellt justerar den sista kvarvarande siffran. Programmerare avrundar för att minska precisionen när exakta värden inte är nödvändiga, hantera flyttalsfel, eller förbereda tal för användarvänlig visning.

## Hur man gör: 
I C använder du vanligtvis funktionerna `floor()`, `ceil()` eller `round()`. Här är en snabb genomgång:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // Floor: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // Ceil: 4.00
    printf("Round: %.2f\n", num_round); // Round: 3.00
    return 0;
}
```

För mer kontroll, som att avrunda till en specifik plats, multiplicerar du, avrundar, och delar:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Avrundat till 2 decimaler: %.2f\n", num_rounded); // Avrundat till 2 decimaler: 3.14
```

## Djupdykning
Förr i tiden innebar avrundning ofta en manuell process—en tung lyft med bara penna och papper. Med datorer automatiserade vi detta, men flyttalsaritmetik förde med sig nyanser på grund av dess binära natur, där vissa tal inte kan representeras exakt.

Alternativ till standardavrundning inkluderar trunkering (att helt enkelt släppa extra siffror) eller bankers avrundning, som avrundar till det närmaste jämna talet när det är exakt mellan två värden, vilket minskar snedvridningen i upprepade beräkningar.

Implementeringen blir knepig när du behöver avrunda tal med godtycklig precision eller hantera specialfall som oändlighet, signalerande NaNs eller subnormala värden. C-standardbiblioteksfunktionerna hanterar grunderna, men om du behöver avrunda decimaler på egna sätt, behöver du mer än `math.h`.

## Se även
- [Dokumentation för `<math.h>`](https://en.cppreference.com/w/c/numeric/math)
- [Flyttalsaritmetik](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
- [Fallgroparna med att verifiera flyttalsberäkningar](https://dl.acm.org/doi/10.1145/1186736.1186737)
