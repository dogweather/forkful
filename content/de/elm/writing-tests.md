---
title:    "Elm: Tests schreiben"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Warum

Automatisierte Tests sind ein wichtiger Bestandteil der modernen Softwareentwicklung. Sie helfen dabei, potenzielle Fehler frühzeitig zu erkennen, die Stabilität und Qualität des Codes zu verbessern und Entwicklungszeit zu sparen. In diesem Artikel werden wir uns anschauen, warum es sich lohnt, Tests in Elm zu schreiben.

## Wie geht das?

Um Tests in Elm zu schreiben, müssen wir zunächst das Paket `elm-test` installieren. Dies können wir einfach über die Kommandozeile mit folgendem Befehl tun:

`elm install elm-explorations/test`

Anschließend können wir unsere Tests innerhalb einer `Tests.elm`-Datei schreiben. Hier ist ein einfaches Beispiel, bei dem wir die `String.reverse`-Funktion testen:

```elm
module Tests exposing (..)

import Expect
import String

reverseTest =
    String.reverse "Hello" == "olleH"
        |> Expect.equal "Reverse of 'Hello' should be 'olleH'"
```

Um die Tests auszuführen, können wir in der Kommandozeile `elm-test` eingeben. Dabei wird unsere `Tests.elm`-Datei automatisch gefunden und die Ergebnisse der Tests ausgegeben.

```
elm-test

======: TEST RUN :======
oke. got "Reverse of 'Hello' should be 'olleH'"
~~~~~~~~~~
PASS :-)
```



## Tiefentauchen

In der Regel schreiben wir Tests, um zu überprüfen, ob unsere Funktionen das tun, was wir von ihnen erwarten. Dabei können wir uns auf verschiedene Arten von Tests konzentrieren, wie zum Beispiel:

- **Unit-Tests**: Diese testen einzelne Funktionen oder Module und überprüfen, ob sie die erwarteten Ergebnisse liefern.
- **Integrationstests**: Diese testen die Zusammenarbeit von mehreren Funktionen oder Modulen und überprüfen, ob sie korrekt miteinander kommunizieren.
- **End-to-End-Tests**: Diese simulieren die tatsächliche Benutzerinteraktion mit der Anwendung und überprüfen, ob alle Komponenten korrekt funktionieren.

Es ist wichtig, einen ausgewogenen Mix aus diesen verschiedenen Arten von Tests zu haben, um eine umfassende Testabdeckung zu erreichen.

Außerdem ist es hilfreich, die sogenannte "Arrange-Act-Assert"-Strategie (auch bekannt als "AAA") zu verwenden, um unsere Tests gut strukturiert und leicht nachvollziehbar zu machen. Dabei teilen wir unsere Tests in drei Abschnitte auf:

- Arrange: Hier bereiten wir die erforderlichen Eingabedaten für unseren Test vor.
- Act: Hier führen wir die Funktion aus, die wir testen möchten.
- Assert: Hier vergleichen wir das erwartete Ergebnis mit dem tatsächlichen Ergebnis.

Durch diese Struktur wird es einfacher, unsere Tests zu debuggen und zu verstehen, was bei fehlerhaften Tests schiefgelaufen ist.

## Siehe auch

- [Offizielle Elm Test Dokumentation](https://package.elm-lang.org/packages/elm-explorations/test/latest/)
- [Elm Unit Testing Tutorial](https://www.codementor.io/@yehonathanbello5/elm-unit-testing-tutorial-dobr5ex9y)
- [Ein Einführungsleitfaden für Integrationstests in Elm](https://blog.mediocregopher.com/2018/01/28/an-intro-guide-to-integration-testing-in-elm/)
- [Automatisierte Tests in Elm mit Travis CI](https://thoughtbot.com/blog/elm-and-travis-ci)