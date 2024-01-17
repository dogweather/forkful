---
title:                "Att skriva tester"
html_title:           "Arduino: Att skriva tester"
simple_title:         "Att skriva tester"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/arduino/writing-tests.md"
---

{{< edit_this_page >}}

# Vad & Varför?

Att skriva tester är en viktig del av programmering för att säkerställa att koden fungerar som den ska. Det är en metod för att identifiera och åtgärda fel innan koden används i produktion.

# Hur man:

Att skriva tester i Arduino är enkelt med hjälp av inbyggda funktioner och bibliotek. Här är ett exempel på en kod som testar en funktion som lägger till två tal och returnerar resultatet:

```Arduino
int add(int x, int y) {
  return x + y;
}
```

För att testa denna funktion kan vi använda "assert" funktionen för att verifiera att det förväntade resultatet returneras:

```Arduino
assert(add(2, 3) == 5);
```

Om det förväntade resultatet inte returneras kommer testet att misslyckas och indikera att det finns ett problem som behöver åtgärdas.

# Djupdyka:

Att skriva tester har blivit en allt vanligare praxis inom programmering sedan början av 2000-talet. Det finns flera verktyg och ramverk som hjälper till att automatiskt köra tester och generera rapporter om eventuella fel.

Ett annat alternativ till att skriva tester är att använda statisk kodanalys, vilket granskar koden utan att faktiskt köra den. Detta kan också vara en effektiv metod för att hitta eventuella fel.

Implementering av tester i Arduino-projekt kan vara en tidsbesparande process men har fördelar på lång sikt genom att minska risken för fel och förenkla felsökning.

# Se även:

[Officiell Arduino dokumentation om enhetstestning](https://www.arduino.cc/en/guide/unit_testing)

[ArdunioTest - ett populärt enhetstestbibliotek för Arduino](https://github.com/leethomason/ArduinoTest)