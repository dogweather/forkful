---
title:                "Verwendung von regulären Ausdrücken"
date:                  2024-01-19
html_title:           "Arduino: Verwendung von regulären Ausdrücken"
simple_title:         "Verwendung von regulären Ausdrücken"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Reguläre Ausdrücke (Regex) sind Muster zur Textsuche und -manipulation. Programmierer nutzen sie, um Textdaten effizient zu durchsuchen, zu validieren oder zu bearbeiten.

## How to:
Arduino unterstützt reguläre Ausdrücke nicht standardmäßig. Abhilfe schafft die Einbindung von Bibliotheken wie `Regexp`. Hier ein Beispiel:

```Arduino
#include <Regexp.h>

void setup() {
    Serial.begin(9600);
    while (!Serial);

    MatchState ms;
    ms.Target ("Das ist ein Test 123.");
    
    char result = ms.Match ("(\\w+)\\s(\\w+)");
  
    if (result == REGEXP_MATCHED) {
        char buf[100];

        ms.GetCapture (buf, 0);
        Serial.println (buf); // "ist"
        
        ms.GetCapture (buf, 1);
        Serial.println (buf); // "ein"
    }
}

void loop() {
    // Nichts zu tun hier.
}
```

Ausgabe:
```
ist
ein
```

## Deep Dive

Reguläre Ausdrücke wurden in den 1950er Jahren konzipiert und sind seitdem fester Bestandteil vieler Programmiersprachen. Arduino hingegen bietet nativ keine Regex-Unterstützung, doch Bibliotheken wie `Regexp` füllen diese Lücke. Diese Bibliotheken bieten meist eine reduzierte Funktionalität im Vergleich zu vollausgestatteten Regex-Engines in Sprachen wie Python oder Java.

Alternativen zu Regex sind gezielte String-Operationen und -Suchfunktionen, die in Arduino selbst eingebaut sind. Diese können einfacher sein, sind aber weniger mächtig und flexibel.

Bei der Implementierung von Regex in Arduino-Projekten muss der begrenzte Speicherplatz berücksichtigt werden. Reguläre Ausdrücke können schnell komplex und ressourcenintensiv werden.

## See Also

- Arduino String Library: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- Regexp-Bibliothek für Arduino: https://github.com/nickgammon/Regexp
- RegexOne – Lernen von Regex mit interaktiven Übungen: https://regexone.com/
