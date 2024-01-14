---
title:    "Elm: Das aktuelle Datum erhalten"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum Du das aktuelle Datum in Elm erhalten solltest
Wenn du mit Elm programmierst, wirst du oft das aktuelle Datum benötigen, um deine Anwendung mit dynamischen Informationen zu versorgen. Zum Beispiel könntest du das aktuelle Datum verwenden, um den Nutzer über Änderungen in deiner App zu informieren oder um personalisierte Inhalte anzuzeigen. In diesem Beitrag werden wir uns anschauen, wie man das aktuelle Datum in Elm erhalten kann.

## Wie Du das aktuelle Datum in Elm erhalten kannst
Um das aktuelle Datum in Elm zu erhalten, können wir die `Time`-Bibliothek verwenden. Diese Bibliothek bietet uns Funktionen, um mit Datums- und Zeitangaben umzugehen. Wir werden uns hier auf die `now`-Funktion konzentrieren, die uns das aktuelle Datum als Tupel mit drei Werten zurückgibt: Jahr, Monat und Tag. Hier ist ein Beispielcode, wie man die `now`-Funktion verwenden kann:

```Elm
import Time exposing (..)

currentDate = now
```

Wenn wir nun `currentDate` ausführen, erhalten wir ein Tupel, das das aktuelle Datum enthält. Hier ist ein Beispiel für den Output:

```Elm
(2021, 10, 25)
```

Um das aktuelle Datum in einer lesbaren Form auszugeben, können wir die `toIsoString`-Funktion verwenden. Diese Funktion gibt uns das Datum als String in ISO-Format zurück. Hier ist ein Beispielcode:

```Elm
import Time exposing (..)

currentDate = now |> toIsoString
```

Der Output wäre dann:

```Elm
"2021-10-25"
```

Es gibt noch weitere Funktionen in der `Time`-Bibliothek, um das aktuelle Datum oder die aktuelle Zeit in verschiedenen Formaten zu erhalten. Eine vollständige Dokumentation findest du <a href="https://package.elm-lang.org/packages/elm/time/latest/">hier</a>.

## Tiefere Einblicke in die Erhaltung des aktuellen Datums
Beim Einsatz von Elm für ein größeres Projekt kann es sinnvoll sein, das aktuelle Datum in ein eigenes Modul auszulagern. Auf diese Weise kannst du das Datum leicht aus dem gesamten Projekt abrufen, ohne es jedes Mal neu importieren zu müssen. Ein weiterer Vorteil ist, dass du dein eigenes Modul mit Zusatzfunktionen versehen und so anpassen kannst, wie du das aktuelle Datum darstellen und ausgeben möchtest.

## Siehe auch
- <a href="https://package.elm-lang.org/packages/elm/time/latest/">Dokumentation zur Time-Bibliothek</a>