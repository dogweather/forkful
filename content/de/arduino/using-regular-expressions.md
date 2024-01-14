---
title:                "Arduino: Verwendung regulärer Ausdrücke"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Warum

Reguläre Ausdrücke sind ein leistungsstarkes Werkzeug für die Textverarbeitung und finden in vielen Programmiersprachen Anwendung. Mit regulären Ausdrücken kann man komplexe Muster in einem Text erkennen und verarbeiten. In der Arduino-Programmierung können sie beispielsweise verwendet werden, um bestimmte Eingaben zu überprüfen oder Daten zu filtern.

## Wie

Um reguläre Ausdrücke in Arduino verwenden zu können, muss zunächst die Bibliothek "Regex" installiert werden. Danach kann man mit der Funktion `regexMatch()` in Kombination mit `RegexMatch()` und `RegexMatchResult()` verschiedene Ausdrücke auf einen Text anwenden.

Ein einfaches Beispiel wäre die Überprüfung einer E-Mail-Adresse mit dem Ausdruck `^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$`. Dieser prüft, ob die E-Mail-Adresse aus einem String mit Buchstaben, Zahlen und Sonderzeichen sowie einem `@` und einem Punkt besteht.

```Arduino
#include <Regex.h>

void setup() {
  Serial.begin(9600);
  String email = "example@test.com";
  RegexMatch match;
  RegexMatchResult result;

  // String mit regulärem Ausdruck überprüfen
  if (regexMatch(email, match, result, "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+$")) {
    Serial.println("Die E-Mail-Adresse ist gültig.");
  } else {
    Serial.println("Die E-Mail-Adresse ist ungültig.");
  }
}

void loop() {

}
```

Die Ausgabe des Codes zeigt, dass die E-Mail-Adresse gültig ist.

```
Die E-Mail-Adresse ist gültig.
```

Es gibt eine Vielzahl von Möglichkeiten, reguläre Ausdrücke in der Arduino-Programmierung einzusetzen. Mit `RegexMatch::matched()` kann beispielsweise überprüft werden, ob ein Pattern in einem Text gefunden wurde. Mit `RegexMatch::replace()` kann ein Ausdruck in einem Text ersetzt werden.

## Deep Dive

Die Verwendung von regulären Ausdrücken erfordert ein Verständnis für die Syntax und Struktur von Patterns. Es gibt verschiedene Zeichen und Sonderzeichen, die eine Bedeutung innerhalb eines regulären Ausdrucks haben. Zum Beispiel werden mit `[a-z]` alle Kleinbuchstaben von a bis z erfasst. Weitere nützliche Zeichen sind `.` für ein beliebiges Zeichen und `+` und `*` für das Vorkommen eines Zeichens.

Um reguläre Ausdrücke effektiv und richtig einzusetzen, kann es hilfreich sein, sich mit dem Konzept des sogenannten "greedy matching" auseinanderzusetzen. Dabei versuchen reguläre Ausdrücke, so viele Zeichen wie möglich zu erfassen und eventuell auch über das gewünschte Muster hinaus zu gehen. Mit dem Zusatz `?` kann man dieses Verhalten verhindern und nur das benötigte Muster erfassen.

In der Arduino-Bibliothek gibt es auch die Möglichkeit, Variablen innerhalb von Patterns zu verwenden. Mit `(?<VariableName>Pattern)` wird beispielsweise eine Variable mit dem Namen "VariableName" definiert, die das erfasste Muster beinhaltet. Diese Variablen können dann in ihrem Code verwendet werden.

## Siehe auch

- [RegExr](https://regexr.com/) - Online-Tool für die Erstellung und Überprüfung von regulären Ausdrücken.
- [Tutorial: Using regular expressions in Arduino](https://www.hackster.io/grahamvickers/using-regular-expressions-in-arduino-db181e) - Ein hilfreiches Tutorial für den Einstieg in die Verwendung von regulären Ausdrücken in Arduino.
- [Arduino Regex Library](https://github.com/marcelotmelo/arduino-regex-library) - Offizielle Bibliothek für die Verwendung von regulären Ausdrücken in Arduino.