---
title:                "Elm: Suchen und Ersetzen von Text"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Warum:
Es gibt viele Gründe, warum man beim Programmieren Text suchen und ersetzen möchte. Man könnte zum Beispiel bestimmte Code-Zeilen oder Variablennamen umbenennen oder Fehler in einer großen Codebasis beheben. Zum Glück gibt es in Elm einfache und effektive Methoden, um Textsuche und -ersetzungen durchzuführen.

Wie geht's:
Die grundlegendste Methode zum Suchen und Ersetzen von Text in Elm ist die `replace`-Funktion. Diese nimmt zwei Argumente an: den zu ersetzenden Text und den Ersatztext. Hier ist ein Beispiel:

```Elm
replace "alt" "neu" "Mein alter Text"
-- Ergebnis: "Mein neuer Text"
```

Das ist jedoch nur die Spitze des Eisbergs. Elm bietet auch erweiterte Möglichkeiten, um Text zu durchsuchen und zu modifizieren. Zum Beispiel können wir die `Regex`-Bibliothek nutzen, um komplexere Suchmuster zu definieren und zu verwenden. Hier ist ein Beispiel für die Verwendung von `Regex.replace`:

```Elm
import Regex

Regex.replace (Regex.regex "\\bhallo\\b") "ciao" "Hallo Welt!"
-- Ergebnis: "ciao Welt!"
```

Tiefer geht's:
Ein wichtiger Faktor beim Suchen und Ersetzen von Text ist die Verwendung von regulären Ausdrücken. Diese ermöglichen es, Muster in Texten zu definieren und darauf zu basierend zu suchen und zu ersetzen. Es gibt viele Ressourcen und Tutorials, die sich mit dem Thema beschäftigen, daher empfehlen wir, sich näher damit zu befassen und regelmäßig zu üben, um ein besseres Verständnis zu erhalten.

Siehe auch:
- [Elm Dokumentationen zu Textsuche und -ersetzung](https://guide.elm-lang.org/text/space/replace.html)
- [Elm-Hilfe zu regulären Ausdrücken](https://guide.elm-lang.org/advanced/regex.html)
- [Online-RegEx Tester](https://regexr.com/) zum Ausprobieren von Suchmustern