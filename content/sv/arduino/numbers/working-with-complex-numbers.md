---
date: 2024-01-26 04:36:49.107608-07:00
description: "Komplexa tal har en reell del och en imagin\xE4r del, vanligtvis skrivet\
  \ som `a + bi`. De \xE4r avg\xF6rande f\xF6r vissa matematikintensiva Arduino-projekt\
  \ som\u2026"
lastmod: '2024-03-13T22:44:38.162192-06:00'
model: gpt-4-0125-preview
summary: "Komplexa tal har en reell del och en imagin\xE4r del, vanligtvis skrivet\
  \ som `a + bi`. De \xE4r avg\xF6rande f\xF6r vissa matematikintensiva Arduino-projekt\
  \ som\u2026"
title: Att arbeta med komplexa tal
---

{{< edit_this_page >}}

## Vad och Varför?
Komplexa tal har en reell del och en imaginär del, vanligtvis skrivet som `a + bi`. De är avgörande för vissa matematikintensiva Arduino-projekt som involverar signalbehandling, elektroteknik eller något annat område där fenomen bäst modelleras i ett plan.

## Hur man gör:
```Arduino
#include <Complex.h>

void setup() {
  Serial.begin(9600); // Starta seriell kommunikation
  
  Complex myComplex(2, 3); // Skapa ett komplext tal 2 + 3i
  Complex anotherComplex(1, 1); // Skapa ett annat komplext tal 1 + 1i
  
  // Addition
  Complex result = myComplex + anotherComplex; 
  Serial.print("Addition: "); 
  result.print(); // Ger ut 3 + 4i
  
  // Multiplikation
  result = myComplex * anotherComplex; 
  Serial.print("Multiplikation: ");
  result.print(); // Ger ut -1 + 5i
}

void loop() {
  // Används inte i detta exempel
}
```
Exempel på utmatning:
```
Addition: 3 + 4i
Multiplikation: -1 + 5i
```

## Fördjupning
Ursprungligen möttes komplexa tal av skepsis, men de har blivit centrala inom olika vetenskapliga fält. Historiskt sett erkändes de för att tillhandahålla lösningar på polynomekvationer som saknar reella lösningar.

Arduino inkluderar inte komplexa tal i sitt standardbibliotek, men du kan dra nytta av bibliotek som `Complex.h` för att hantera dem. Internt definierar dessa bibliotek en Complex-klass, vanligtvis med användning av två doubles för att lagra den reella och imaginära delen, och överbelastar operatorer för att stödja aritmetik.

Som ett alternativ, för applikationer som inte inneboende behöver aritmetik med komplexa tal, överväg att använda andra matematiska strategier eller bibliotek. Kom dock ihåg att användning av floats istället för komplexa tal kan förenkla vissa problem.

## Se också
- [Complex.h](https://github.com/RobTillaart/Complex)-biblioteket av Rob Tillaart.
- En djupare dykning i [matematiken bakom komplexa tal](https://mathworld.wolfram.com/ComplexNumber.html).
