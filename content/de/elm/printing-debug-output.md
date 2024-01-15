---
title:                "Debug Ausgabe drucken"
html_title:           "Elm: Debug Ausgabe drucken"
simple_title:         "Debug Ausgabe drucken"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man debug Ausgaben in Elm verwenden? Ganz einfach: Es kann dir helfen, Fehler schneller zu finden und zu beheben! Durch das Hinzufügen von Debug Ausgaben kannst du besser verstehen, wie deine Code funktioniert und wo mögliche Probleme auftreten.

## Wie geht das?

Es gibt verschiedene Möglichkeiten, Debug Ausgaben in deinem Elm Code zu erstellen.

Die einfachste Methode ist die Verwendung der `Debug.log` Funktion. Diese Funktion akzeptiert zwei Argumente: einen Debug-Namen und einen Wert. Zum Beispiel, um die Variable `x` auszugeben, kannst du Folgendes tun:

```Elm
x = 5

Debug.log "x" x
```

Wenn du diese Funktion in deinem Code verwendest, wird der Wert von `x` in der Konsole ausgegeben, wenn du deine Anwendung ausführst. Diese Methode ist besonders nützlich, um den Wert von Variablen in verschiedenen Teilen deines Codes zu überwachen.

Eine andere Möglichkeit ist die Verwendung von `Debug.logEvery` Funktion. Diese Funktion funktioniert ähnlich wie `Debug.log`, aber sie wird jedes Mal ausgeführt, wenn sich die aktualisiert wird. Sie akzeptiert auch einen Debug-Namen und einen Wert als Parameter.

```Elm
x = "Hello"

Debug.logEvery "x" x
```

Schließlich gibt es noch die `Debug.watch` Funktion, die es dir ermöglicht, verschiedene Debug-Namen und Werte in einem Objekt zu sammeln und sie alle gleichzeitig auszugeben.

```Elm
x = 5
y = "World"

Debug.watch "Test" { x, y }
```

Die Funktion `Debug.watch` nimmt ein Objekt als Argument entgegen und gibt alle enthaltenen Debug-Namen und Werte aus.

## Tiefentauchen

Neben den grundlegenden Debug-Funktionen gibt es noch weitere Möglichkeiten, Debug Ausgaben in Elm zu nutzen. Zum Beispiel kannst du deine eigenen Debug-Funktionen erstellen, die spezifisch auf deine Bedürfnisse zugeschnitten sind. Dies ermöglicht dir noch mehr Kontrolle über deine Debug Ausgaben und eröffnet neue Möglichkeiten, um Probleme zu identifizieren und zu beheben.

Eine andere nützliche Technik ist die Verwendung von Debug Flags, die es dir ermöglichen, Debug Ausgaben in bestimmten Umgebungen ein- oder auszuschalten. Auf diese Weise kannst du deine Debug Ausgaben entfernen, wenn du deine Anwendung in der Produktion ausführst, aber sie aktiviert lassen, wenn du an deiner Anwendung in der Entwicklungsumgebung arbeitest.

## Siehe auch

- [Offizielle Elm Dokumentation zu Debug Ausgaben](https://guide.elm-lang.org/debugging/debugging.html)
- [Debug Ausgaben in Elm: Ein umfassender Leitfaden](https://www.codementor.io/@ajaybelhaj/printing-debug-output-in-elm-tutorial-ozc0gly3b) von Ajay Belhaj
- [Debugging Elm in der Praxis](https://medium.com/front-end-hacking/debugging-elm-in-practice-6bb1f9aead82) von Tomasz Kaczmarzyk