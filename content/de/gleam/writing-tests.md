---
title:                "Gleam: Tests schreiben"
programming_language: "Gleam"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

In der Welt des Programmierens ist es wichtig, effiziente und fehlerfreie Code zu schreiben. Eine bewährte Methode, um sicherzustellen, dass der Code einwandfrei funktioniert und verwendbar ist, sind Tests. In diesem Beitrag werden wir uns genauer anschauen, warum das Schreiben von Tests in Gleam von Vorteil ist.

## Wie

Gleam bietet eine eingebaute Testbibliothek, die das Schreiben von Tests leicht und effizient macht. Zunächst müssen wir diese in unserem Programm importieren:

```Gleam
import gleam/test
```

Als nächstes definieren wir unsere eigentlichen Tests, indem wir eine Funktion schreiben und sie mit der Attribut `#[test]` versehen:

```Gleam
#[test]
pub fn add_test() {
  assert.equal(10, add(6, 4))
}

#[test]
pub fn subtract_test() {
  assert.equal(2, subtract(8, 6))
}
```

Hier sehen wir, dass wir mit dem `assert.equal`-Befehl die erwarteten Ergebnisse unserer Funktionen überprüfen können. Wir können auch weitere Assertions hinzufügen, um sicherzustellen, dass unsere Funktionen in verschiedenen Szenarien richtig funktionieren.

Um unsere Tests auszuführen, verwenden wir den Befehl `gleam test` in der Konsole. Wenn alle Tests erfolgreich sind, sehen wir eine Erfolgsmeldung. Wenn ein Test fehlschlägt, werden wir über den Grund informiert und können unseren Code entsprechend anpassen.

## Deep Dive

Jetzt, wo wir wissen, wie man Tests in Gleam schreibt, werden wir tiefer in das Thema eintauchen. Hier sind einige Tipps, die beim Schreiben von Tests hilfreich sein können:

- Schreibe Tests, bevor du den entsprechenden Code entwickelst. Auf diese Weise weißt du sofort, ob dein Code richtig funktioniert, sobald er fertig ist.
- Vermeide es, Daten oder Funktionen direkt in deinen Tests zu verwenden. Stattdessen sollten wir Mock-Daten oder -Funktionen erstellen, um sicherzustellen, dass unsere Tests unabhängig von anderen Teilen des Codes sind.
- Führe regelmäßig Tests durch, besonders wenn du neue Funktionen hinzufügst oder vorhandenen Code änderst, um sicherzustellen, dass keine unerwarteten Fehler auftreten.

Das Schreiben von effektiven Tests kann dazu beitragen, die Qualität und Zuverlässigkeit deines Codes zu verbessern und dazu beitragen, Probleme frühzeitig zu erkennen und zu beheben.

## Siehe auch

- [Gleam Dokumentation über Tests](https://gleam.run/book/testing.html)
- [Fünf Tipps für das Schreiben von guten Tests](https://www.oreilly.com/library/view/five-tips-for/9781491935223/)
- [Warum Tests so wichtig sind](https://www.softwaretestingnews.co.uk/why-testing-important/)