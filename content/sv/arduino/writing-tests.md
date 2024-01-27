---
title:                "Skriva tester"
date:                  2024-01-19
html_title:           "Arduino: Skriva tester"
simple_title:         "Skriva tester"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-tests.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva tester innebär att koda checks som verifierar att din kod fungerar som förväntat. Programmerare gör detta för att hitta buggar tidigt, förbättra kodkvaliteten och försäkra sig om att framtida ändringar inte bryter funktionalitet.

## Steg för steg:
Arduino saknar ett inbyggt testramverk, men vi kan ändå implementera enkla tester. Här är ett exempellösning:

```cpp
void setup() {
  Serial.begin(9600);
  testFunction();
}

void loop() {
  // Normala loop-aktiviteter här.
}

void testFunction() {
  int resultat = addera(5, 10);
  if (resultat == 15) {
    Serial.println("Test passed!");
  } else {
    Serial.println("Test failed!");
  }
}

int addera(int a, int b) {
  return a + b;
}
```

Förväntad utskrift:
```
Test passed!
```

## Djupdykning:
Arduino är mer känt för dess hårdvarunära programmering än för testdriven utveckling. På senare tid har dock intresset för att skriva tester för Arduino-kod ökat, och communityn har skapat egna verktyg och bibliotek som till exempel `ArduinoUnit` för att underlätta detta. Historiskt sett var tester mer manuell kontroll av fysisk hårdvara, men nu kan vi testa logik direkt i mjukvaran. Andra alternativ för inbyggda system inkluderar hårdvarusimulering och modulbaserade tester.

## Se även:
- ArduinoUnit på GitHub: https://github.com/mmurdoch/arduinounit
- Officiell Arduino's början på testning: https://www.arduino.cc/en/Guide/Environment#toc8
- Testdriven utveckling (TDD) introduktion: https://en.wikipedia.org/wiki/Test-driven_development

Observera att länkarna är till engelskspråkiga resurser då mycket av den djupgående informationen om Arduino fortfarande är på engelska.
