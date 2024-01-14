---
title:    "Arduino: Att skriva tester"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

Readers, varför är det viktigt att skriva tester för din Arduino-kod?

När du arbetar med att utveckla Arduino-projekt är det viktigt att använda en testdriven utvecklingsmetodik. Tester hjälper dig att identifiera och fixa eventuella fel i din kod innan den implementeras på din hårdvara. Det sparar inte bara tid utan även pengar och frustration på lång sikt.

## Hur man skriver tester för Arduino

Det finns flera olika sätt att skriva tester för din Arduino-kod. Ett vanligt sätt är att använda programvarubibliotek som Arduino Unit eller AUnit, som är speciellt utformade för att testa Arduino-kod. Du kan också använda en integrerad utvecklingsmiljö (IDE) som stöder inbyggda enhetstester för att skapa och köra dina tester.

Här är ett exempel på hur du kan skriva ett enkelt enhetstest för en funktion som adderar två tal i Arduino:

```Arduino
#include <ArduinoUnit.h>

int addition(int num1, int num2) {
  return num1 + num2;
}

test(addition) {
  assertEqual(addition(2, 3), 5);  //testar adderingsfunktionen
}

ArduinoUnitOnce();  //kör alla tester en gång
```

När testet körs kommer utmatningen att visa om testet har lyckats eller misslyckats. Om det misslyckas kan du sedan gå igenom koden och identifiera var felet ligger.

## Fördjupning i att skriva tester för Arduino

Att skriva tester för din Arduino-kod är ett viktigt steg för att säkerställa att din kod fungerar korrekt och pålitligt. När du skriver tester bör du fokusera på att täcka olika scenarier och använda olika metoder för att testa din kod. Det är också viktigt att använda en enhetstestsvit som är lätt att underhålla och uppdatera i framtiden.

Ett annat viktigt aspekt av att skriva tester är att vara noggrann med din kod. Tester kan bara identifiera fel om din kod är korrekt och redan testad. Genom att noggrant gå igenom din kod och testa den i små delar kan du undvika onödiga fel och problem när det kommer till att implementera den på hårdvaran.

## Se även

- [Arduino Unit biblioteket](https://github.com/mmurdoch/arduinounit)
- [AUnit biblioteket](https://github.com/bxparks/AUnit)
- [Enhetstestning i Arduino IDE](https://www.arduino.cc/en/Tutorial/TestingWithArduinounit)