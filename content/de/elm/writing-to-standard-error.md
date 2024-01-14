---
title:    "Elm: Schreiben nach Standard-Fehler"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum

Warum sollte man überhaupt versuchen, etwas auf die Standardfehlerausgabe zu schreiben? Die Antwort ist einfach: Es ist ein wichtiges Instrument beim Programmieren, um Fehlermeldungen und Warnungen zu erfassen und zu untersuchen.

## Wie geht man vor?

Um etwas auf die Standardfehlerausgabe zu schreiben, gibt es verschiedene Methoden, je nachdem welche Art von Programm man schreibt. Hier ist ein Beispiel in Elm:

```Elm
import Debug exposing (crash)
 
crash "Es ist ein Fehler aufgetreten!"
```

Die Ausgabe würde dann folgendermaßen aussehen:

```Shell
Error: Es ist ein Fehler aufgetreten!
```

Natürlich kann man auch andere Methoden verwenden, je nachdem welche Art von Programmierumgebung oder Situation man hat.

## Tiefergehende Informationen

Es gibt viele Gründe, warum man standardmäßig auf die Standardfehlerausgabe schreiben sollte. Zum einen kann es helfen, bei der Fehlersuche oder der Überwachung der Anwendung zu helfen. Außerdem ist es auch eine gute Möglichkeit, um Informationen über den internen Zustand des Programms zu erhalten.

Das Schreiben auf die Standardfehlerausgabe kann auch bei der Entwicklung und Verbesserung der Anwendung helfen, da es eine einfache Möglichkeit ist, um Daten oder Variablenwerte zu überprüfen.

## Siehe auch

- [Elm Fehlerbehandlung](https://guide.elm-lang.org/error_handling/)
- [Debugging Elm Code](https://elmprogramming.com/debugging-elm-code.html)
- [Einfacheres Debuggen in Elm mit TeaSpoon](https://cronokirby.com/posts/elm/2016-11-28-intro-teaspoon.html)