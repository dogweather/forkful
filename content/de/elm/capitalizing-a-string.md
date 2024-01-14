---
title:                "Elm: Ein String großschreiben"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie sich schon einmal gefragt haben, wie Sie in Elm eine Zeichenkette in Großbuchstaben umwandeln können, dann sind Sie hier richtig. In diesem Blogbeitrag werden wir uns genauer ansehen, wie man eine Zeichenkette in Elm capitalisieren kann.

## Wie geht das?

Um eine Zeichenkette in Elm zu capitalisieren, können Sie die eingebaute Funktion `String.toUpper` verwenden. Diese Funktion nimmt eine Zeichenkette entgegen und gibt eine neue Zeichenkette zurück, in der alle Buchstaben in Großbuchstaben umgewandelt wurden. Hier ist ein Beispiel:

```Elm
String.toUpper "hallo"
```
Dieser Code würde als Ausgabe `"HALLO"` produzieren.

Natürlich funktioniert diese Methode nur mit alphabetischen Zeichen. Wenn Sie andere Zeichen wie Leerzeichen oder Sonderzeichen in einer Zeichenkette haben, werden diese nicht verändert. Hier ist ein weiteres Beispiel, um das zu verdeutlichen:

```Elm
String.toUpper "hello world"
```

Die Ausgabe hier wäre immer noch `"HELLO WORLD"`, da die Leerzeichen nicht verändert werden.

## Tiefere Einblicke

Wenn Sie sich fragen, wie die `String.toUpper` Funktion genau funktioniert, gibt es einige tiefere Einblicke, die wir hier teilen können. Diese Funktion nutzt die integrierte `String.map` Funktion, um jeden Buchstaben in der Zeichenkette zu verarbeiten. Dies bedeutet, dass Sie auch die `String.map` Funktion selbst nutzen könnten, um eine ähnliche Funktion zu erstellen, die Ihre Zeichenkette auf eine andere Art und Weise capitalisiert.

Es gibt auch andere Möglichkeiten, eine Zeichenkette in Elm zu capitalisieren, wie zum Beispiel durch die Verwendung von regulären Ausdrücken. Dies ist jedoch komplexer und erfordert möglicherweise weitere Kenntnisse in der Programmiersprache. Wenn Sie jedoch auf der Suche nach mehr Flexibilität sind, ist dies möglicherweise eine Option, die Sie ausprobieren möchten.

## Siehe auch

- Offizielle Elm-Referenz für `String.toUpper` Funktion: https://package.elm-lang.org/packages/elm/core/latest/String#toUpper
- Dokumentation zur `String.map` Funktion: https://package.elm-lang.org/packages/elm/core/latest/String#map
- Tutorial zur Verwendung von regulären Ausdrücken in Elm: https://github.com/elm-explorations/regex

Vielen Dank für das Lesen dieses Blogbeitrags. Wir hoffen, er hat Ihnen geholfen, besser zu verstehen, wie Sie in Elm eine Zeichenkette capitalisieren können. Wenn Sie weitere Fragen haben oder weitere Ressourcen benötigen, zögern Sie nicht, die oben genannten Links zu besuchen oder in der Elm-Community nach Unterstützung zu fragen. Bis zum nächsten Mal!