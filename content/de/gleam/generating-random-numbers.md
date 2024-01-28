---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-27T20:34:07.937091-07:00
model:                 gpt-4-0125-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Generieren von Zufallszahlen in der Programmierung kann entscheidend sein für die Erstellung von Simulationen, Testverfahren, Kryptografie und Spielen. In Gleam ist es eine Funktion, die es Entwicklern ermöglicht, Unvorhersehbarkeit einzuführen oder reale Szenarien in ihren Anwendungen zu simulieren.

## Wie:

Um Zufallszahlen in Gleam zu generieren, verwendet man hauptsächlich die `gleam_random` Bibliothek. Diese Bibliothek bietet Funktionen zur Generierung von zufälligen Ganzzahlen, Fließkommazahlen und mehr. Stellen Sie zunächst sicher, dass Sie `gleam_random` in Ihrer `rebar.config` oder `mix.exs` Datei als Abhängigkeit hinzugefügt haben.

Lassen Sie uns in einige Beispiele eintauchen:

### Generierung einer zufälligen Ganzzahl

Um eine zufällige Ganzzahl in einem bestimmten Bereich zu generieren, können Sie die Funktion `int` verwenden:

```gleam
import gleam/random

pub fn generate_random_int() {
  let random_int = random.int(1, 10)
  random_int
}
```

Diese Funktion generiert eine zufällige Ganzzahl zwischen 1 und 10 einschließlich.

### Generierung einer zufälligen Fließkommazahl

Um eine zufällige Fließkommazahl zu erhalten, verwenden Sie die Funktion `float`. Dies generiert eine Fließkommazahl zwischen 0.0 und 1.0:

```gleam
import gleam/random

pub fn generate_random_float() {
  let random_float = random.float()
  random_float
}
```

### Beispiel Ausgabe

Das Ausführen dieser Funktionen könnte Ausgaben wie folgt ergeben:

- Für `generate_random_int()`: `5`
- Für `generate_random_float()`: `0,84372`

Denken Sie daran, dass jede Ausführung aufgrund der Natur der Zufälligkeit zu unterschiedlichen Ausgaben führen kann.

## Tiefergehender Einblick

Das `gleam_random` Modul implementiert einen Pseudozufallszahlengenerator (PRNG), was im Wesentlichen bedeutet, dass die Zahlen nicht wirklich zufällig sind, aber schwer vorherzusagen, und somit Zufälligkeit emulieren. PRNGs funktionieren, indem sie mit einem Anfangswert beginnen, bekannt als der Seed, und mathematische Operationen anwenden, um eine Sequenz von Zahlen zu generieren.

Historisch gesehen haben Sprachen und Bibliotheken mehrere Algorithmen für PRNGs implementiert, wie den Mersenne Twister oder den Linearen Kongruenzgenerator (LCG). Die Wahl des Algorithmus beeinflusst die Qualität der "Zufälligkeit", wobei einige besser für kryptografische Anwendungen geeignet sind als andere. Obwohl die Standardbibliothek von Gleam mit ihrem `gleam_random` Modul Bequemlichkeit und Benutzerfreundlichkeit bietet, ist sie möglicherweise nicht immer die beste Wahl für Anwendungsfälle, die kryptografisch sichere Zufälligkeit erfordern. Für kryptografische Zwecke sollten Entwickler in Bibliotheken suchen, die speziell darauf ausgelegt sind, kryptografisch sichere Pseudozufallszahlengeneratoren (CSPRNGs) zu bieten, die darauf ausgelegt sind, Angriffen zu widerstehen, die zukünftige Zahlen vorhersagen könnten, indem sie eine Sequenz von generierten Zahlen beobachten.

Zusammenfassend ist die Zufallszahlengenerierungsfunktionalität von Gleam robust für allgemeine Programmieranforderungen, aber Anwendungen mit spezifischen Sicherheitsanforderungen sollten dedizierte kryptografische Lösungen in Betracht ziehen, um die Integrität und Sicherheit ihrer Zufallszahlengenerierung zu gewährleisten.
