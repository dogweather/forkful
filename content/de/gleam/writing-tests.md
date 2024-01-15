---
title:                "Tests schreiben"
html_title:           "Gleam: Tests schreiben"
simple_title:         "Tests schreiben"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich die Mühe machen, Tests zu schreiben? Nun, Tests sind ein wichtiger Bestandteil der Softwareentwicklung. Sie helfen dabei, unsere Codebase stabil zu halten und Fehler frühzeitig zu erkennen. Das spart Zeit und Nerven in der Zukunft.

## Wie geht das?

Um Tests in Gleam zu schreiben, müssen wir zuerst das `gleam/test`-Paket installieren. Dann können wir unsere Tests in einer separaten `tests`-Datei innerhalb unseres Projekts schreiben.

```Gleam
import gleam/test

suite =
  test.suite("Mein Test Suite", [
    test.test("Addition", {
      expect.i32_to_be(test) {
        assert.equal(2, 1 + 1)
      }
    })
  ])

test.run(suite)
```

Dieses Beispiel zeigt, wie wir eine Test-Suite erstellen und einen einfachen Testfall hinzufügen. Wir importieren das `gleam/test`-Paket, erstellen eine Test-Suite mit einem aussagekräftigen Namen und fügen dann einen Testfall hinzu. Wir erwarten, dass `1 + 1` gleich `2` ergibt und nutzen die `assert.equal`-Funktion, um dies zu überprüfen.

## Tiefer Einblick

Tests sind eine Möglichkeit, um sicherzustellen, dass unser Code wie erwartet funktioniert. Sie können auch helfen, Logikfehler aufzudecken oder sicherstellen, dass Änderungen an einer Funktion keine unbeabsichtigten Nebenwirkungen haben.

Um effektive Tests zu schreiben, sollten wir uns auf möglichst kleine und isolierte Tests konzentrieren. Wir sollten auch sicherstellen, dass unsere Tests immer erfolgreich sind, um sicherzustellen, dass unser Code reibungslos läuft.

## Siehe auch

- [Dokumentation zum `gleam/test`-Paket](https://gleam.run/packages/gleam/test)
- [Einführung in Gleam Testen](https://medium.com/@gleamlang/gleam-test-your-code-with-the-power-of-gleam-7fca5358afa9)
- [Gleam-Community auf Discord](https://discord.gg/VHTr9rwk8a)