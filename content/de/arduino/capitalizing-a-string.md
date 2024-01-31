---
title:                "String in Großbuchstaben umwandeln"
date:                  2024-01-19
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Großschreiben eines Strings bedeutet, jeden Buchstaben darin in Großbuchstaben umzuwandeln. Programmierer nutzen das, um Textdaten einheitlich zu gestalten und beispielsweise Überschriften oder Schaltflächenbeschriftungen hervorzuheben.

## Anleitung:

```Arduino
void setup() {
  Serial.begin(9600);
  char text[] = "Das ist ein Teststring.";
  capitalizeString(text);
  Serial.println(text); // Gibt aus: "DAS IST EIN TESTSTRING."
}

void loop() {
  // Hier könnte Ihre kontinuierliche Logik stehen
}

void capitalizeString(char* str) {
  for (; *str; str++) {
    if (*str >= 'a' && *str <= 'z') {
      *str = *str - 'a' + 'A';
    }
  }
}
```

## Hintergrundwissen:

Die Großschreibung von Strings ist seit den Anfängen des Programmierens ein Standardverfahren. Es gibt verschiedene Wege, die Umwandlung zu implementieren, wie beispielsweise ASCII-Wert-Manipulation oder die Verwendung von Standardfunktionen in Hochsprachen. In C basierten Sprachen wie Arduino ist der direkte Zugriff auf Zeichen und ihre ASCII-Werte eine effiziente Methode. Alternativen dazu könnten höher abstrahierte Funktionen sein, die jedoch möglicherweise mehr Speicherplatz beanspruchen, was bei Mikrocontrollern kritisch sein kann.

## Siehe Auch:

- Arduino String Reference: https://www.arduino.cc/reference/en/language/variables/data-types/string/
- ASCII-Tabelle: https://www.asciitable.com/
- Anleitungen zur String-Manipulation in C: https://www.cprogramming.com/tutorial/c/lesson6.html
