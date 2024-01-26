---
title:                "Code in Funktionen organisieren"
date:                  2024-01-26T01:10:04.709895-07:00
model:                 gpt-4-1106-preview
simple_title:         "Code in Funktionen organisieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Was & Warum?
Code in Funktionen zu organisieren, bedeutet, das Verhalten eines Programms in kleinere, wiederverwendbare Teile zu untergliedern. Programmierer tun dies, um den Code klarer, wartbarer zu machen und um Wiederholungen zu vermeiden.

## Wie geht das:
Hier ist ein einfaches Beispiel für die Organisation von Code in Funktionen in Gleam:

```gleam
fn add(x, y) {
  x + y
}

fn main() {
  let summe = add(3, 4)
  summe
}

// Beispiel-Ausgabe
// 7
```

In diesem Ausschnitt ist `add` eine Funktion, die zwei Werte nimmt und sie addiert. `main` ist der Ort, an dem wir `add` aufrufen und das Ergebnis verarbeiten.

## Tiefergehende Betrachtung
Historisch gesehen hat das Konzept von Funktionen (oder 'Unterroutinen') die Programmierung revolutioniert und den Weg für die strukturierte Programmierung in den 1960er Jahren und darüber hinaus geebnet. Funktionen fördern einen modularen Ansatz, bei dem Probleme in Teilprobleme unterteilt werden, die unabhängig gelöst und zusammengesetzt werden, um das größere Problem zu lösen.

In Gleam, einer streng typisierten Sprache, tragen die Funktionen auch Typinformationen, was sicherstellt, dass ihre Verwendung konsistent mit ihrer Definition ist. Dies verringert Fehler und klärt Absichten.

Alternativen zu Funktionen beinhalten Inline-Coding, wo die Logik wiederholt ausgeschrieben wird. Obwohl dies manchmal schneller für kleine, einmalige Aufgaben ist, skaliert Inline-Coding nicht gut für größere Anwendungen.

Bei der Organisation in Funktionen zu berücksichtigende Implementierungsdetails können die Funktionskomposition umfassen, bei der Funktionen als Bausteine verwendet werden, und höhere Funktionen, die andere Funktionen als Argumente nehmen oder sie zurückgeben, was der Art und Weise, wie Code organisiert und ausgeführt wird, Flexibilität verleiht.

## Siehe auch
Für mehr Informationen zu Funktionen in Gleam, können Sie in die offizielle Dokumentation eintauchen:
- [Gleam language functions](https://gleam.run/book/tour/functions.html)

Oder erkunden Sie breitere Programmierkonzepte:
- [Mozilla Developer Network zu JavaScript-Funktionen](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions)
- [Learn You Some Erlang for Great Good! - Über Module und Funktionen](https://learnyousomeerlang.com/modules)