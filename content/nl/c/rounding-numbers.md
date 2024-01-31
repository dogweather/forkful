---
title:                "Afronden van getallen"
date:                  2024-01-28T22:06:27.284728-07:00
model:                 gpt-4-0125-preview
simple_title:         "Afronden van getallen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/rounding-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het afronden van getallen betekent het afsnijden van de cijfers na een bepaald punt, terwijl eventueel het laatste bewaarde cijfer wordt aangepast. Programmeurs ronden af om de nauwkeurigheid te verminderen wanneer exacte waarden niet noodzakelijk zijn, om te gaan met fouten in floating-point getallen, of om getallen voor te bereiden voor een gebruiksvriendelijke weergave.

## Hoe te:
In C gebruik je typisch de functies `floor()`, `ceil()`, of `round()`. Hier is een snelle demonstratie:

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

Voor meer controle, zoals afronden op een specifieke plaats, vermenigvuldig je, rond je af en deel je:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Afgerond op 2 decimalen: %.2f\n", num_rounded); // Afgerond op 2 decimalen: 3.14
```

## Diepe Duik
Vroeger betekende afronden vaak een handmatig proces - een zware taak met alleen pen en papier. Met computing hebben we dit geautomatiseerd, maar floating-point rekenkunde bracht nuances met zich mee vanwege de binaire aard, waar sommige getallen niet precies kunnen worden weergegeven.

Alternatieven voor standaardafronding zijn onder meer truncatie (simpelweg het laten vallen van extra cijfers) of bankers' rounding, wat afrondt op het dichtstbijzijnde even getal wanneer het precies tussen twee waarden in staat, waardoor de vooringenomenheid bij herhaalde berekeningen wordt verkleind.

Implementatie wordt lastig wanneer je moet afronden op willekeurige nauwkeurige getallen of speciale gevallen zoals oneindigheid, signalerende NaNs, of subnormale waarden moet behandelen. De standaardbibliotheekfuncties van C behandelen de basis, maar als je decimalen op maat moet afronden, heb je meer nodig dan `math.h`.

## Zie Ook
- [Documentatie van `<math.h>`](https://en.cppreference.com/w/c/numeric/math)
- [Floating-point rekenkunde](https://en.wikipedia.org/wiki/Floating-point_arithmetic)
- [De valkuilen van het verifiëren van floating-point berekeningen](https://dl.acm.org/doi/10.1145/1186736.1186737)
