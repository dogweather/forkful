---
title:                "Code in Funktionen organisieren"
date:                  2024-01-26T01:08:19.278451-07:00
model:                 gpt-4-1106-preview
simple_title:         "Code in Funktionen organisieren"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Code in Funktionen zu organisieren bedeutet, den Code in wiederverwendbare Einheiten aufzuteilen, wobei jede Einheit eine spezifische Aufgabe erledigt. Programmierer tun dies, um den Code leichter lesbar, debuggbar und wiederverwendbar zu machen. Es ist wie das Sortieren von Legosteinen in Behälter - es erspart Ihnen das Durcheinanderkramen in einem chaotischen Haufen, jedes Mal wenn Sie etwas bauen wollen.

## Wie:
Stellen Sie sich vor, Sie möchten eine LED blinken lassen. Ohne Funktionen ist Ihre `loop` ein unordentliches Durcheinander. Mit Funktionen ist es übersichtlich. Hier ist wie:

```Arduino
const int LED_PIN = 13;

void setup() {
  pinMode(LED_PIN, OUTPUT);
}

void loop() {
  blinkLED(500); // Die LED alle 500ms blinken lassen
}

// Funktion um eine LED blinken zu lassen
void blinkLED(int delayTime) {
  digitalWrite(LED_PIN, HIGH);
  delay(delayTime);
  digitalWrite(LED_PIN, LOW);
  delay(delayTime);
}
```

Beispielausgabe: Ihre LED blinkt fröhlich vor sich hin, und der Zweck des Codes ist auf einen Blick klar.

## Tiefer Eintauchen
Vor Funktionen war die Programmierung eine lineare Reise; man sah jedes Schlagloch von Anfang bis Ende. Nach Funktionen ist es eher wie Flughüpfen - man springt zu den wichtigen Teilen. Historisch gesehen waren Unterprogramme (frühe Funktionen) eine Revolution in der Programmierung, die es Codierern erlaubten, sich nicht zu wiederholen – das ist das DRY-Prinzip, Don't Repeat Yourself (Wiederhole dich nicht). Alternativen zu Funktionen könnten Makros oder die Nutzung von Klassen für die objektorientierte Programmierung (OOP) umfassen. Die Feinheiten? Wenn Sie eine Funktion definieren, geben Sie dem Compiler eine Blaupause für die Ausführung einer Aufgabe. Mit Arduino definieren Sie oft void-Funktionen, die als einfache Befehle für einen Mikrocontroller dienen, aber Funktionen können auch Werte zurückgeben, was sie vielseitiger macht.

## Siehe auch
Für mehr über Funktionen, schauen Sie durch diese:

- Die offizielle Funktionsreferenz von Arduino: https://www.arduino.cc/reference/en/language/functions/
- Erfahren Sie mehr über das DRY-Prinzip: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
- Auffrischung zur Geschichte von Unterprogrammen: https://en.wikipedia.org/wiki/Subroutine
