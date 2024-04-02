---
date: 2024-01-20 17:37:45.392595-07:00
description: "Das Umwandeln eines Strings in Kleinbuchstaben bedeutet, alle Buchstaben\
  \ des Strings in ihre entsprechenden Kleinbuchstabenversionen zu konvertieren.\u2026"
lastmod: '2024-03-13T22:44:54.132348-06:00'
model: gpt-4-1106-preview
summary: "Das Umwandeln eines Strings in Kleinbuchstaben bedeutet, alle Buchstaben\
  \ des Strings in ihre entsprechenden Kleinbuchstabenversionen zu konvertieren.\u2026"
title: Umformung eines Strings in Kleinbuchstaben
weight: 4
---

## Was & Warum?
Das Umwandeln eines Strings in Kleinbuchstaben bedeutet, alle Buchstaben des Strings in ihre entsprechenden Kleinbuchstabenversionen zu konvertieren. Programmierer nutzen dies, um die Groß-/Kleinschreibung zu vereinheitlichen, meist für Vergleiche oder Datenverarbeitung.

## So geht's:
Hier ist ein einfaches Beispiel, wie du einen String in Kleinbuchstaben umwandelst:

```arduino
void setup() {
  Serial.begin(9600);
  String text = "HeLLo WoRLd!";
  text.toLowerCase();
  Serial.println(text);
}

void loop() {
  // Hier könnte dein Code stehen, der wiederholt ausgeführt wird.
}
```

Wenn du diesen Code auf deinem Arduino ausführst, erhältst du folgende Ausgabe:
```
hello world!
```

## Tiefere Einblicke:
Frühere Programmiersprachen boten oft integrierte Funktionen oder Methoden an, um die Groß-/Kleinschreibung eines Strings zu ändern. In C und C++, den Vorgängern von Arduino, gab es keine eingebaute Methode, was bedeutete, dass Entwickler ihre eigenen Funktionen schreiben mussten. Arduino vereinfacht dies durch die Methode `toLowerCase()`. 

Alternativen hierzu könnten beispielsweise die manuelle Iteration durch den String und die Anwendung des `tolower()` Befehls aus der C Standard-Bibliothek für jedes Zeichen sein. Die `toLowerCase()` Methode von Arduino ist jedoch wegen ihrer Einfachheit und Lesbarkeit empfehlenswert.

Bei der Implementierung durchläuft `toLowerCase()` intern den String und wendet die Groß-zu-Kleinschreibung für jedes Zeichen an. Es ist bemerkenswert, dass die Original-Stringdaten geändert werden, da `toLowerCase()` die Stringinstanz mutiert, auf der sie aufgerufen wird.

## Siehe auch:
- Die Arduino Referenz für die `String` Klasse: https://www.arduino.cc/reference/de/language/variables/data-types/stringobject/
- C++ `tolower` Methode in der cctype Bibliothek: https://en.cppreference.com/w/cpp/string/byte/tolower
- Eine Anleitung zum Arduino String Handling: https://www.arduino.cc/reference/de/language/variables/data-types/string/functions/
