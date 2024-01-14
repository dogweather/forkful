---
title:    "Elm: Vergleich von zwei Daten"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum man sich mit der Programmiersprache Elm beschäftigen sollte, und das Vergleichen von zwei Daten ist definitiv einer davon. Mit diesem Artikel möchte ich Ihnen zeigen, warum dies eine wichtige Fähigkeit ist, die Sie beherrschen sollten.

## Wie geht man vor?

Um zwei Daten in Elm zu vergleichen, gibt es verschiedene Methoden, aber die einfachste ist die Verwendung des inbuilt `Time.compare` Moduls. Hier ist ein Beispiel, wie man zwei Daten mit dieser Methode vergleichen kann:

```Elm
import Time

date1 = Time.millisToPosix 1564262400000
date2 = Time.millisToPosix 1564262460000

result = Time.compare date1 date2

-- result wird -1 zurückgeben, da date1 vor date2 liegt
```

In diesem Beispiel habe ich zwei Millisekundenzeitstempel zum einfacheren Verständnis verwendet, aber Sie können auch andere Datentypen verwenden, wie z.B. `Time.Posix`, `Time.Year`, `Time.Month`, etc. `Time.compare` gibt einen Integer zurück, der -1, 0 oder 1 sein kann, je nachdem, ob die erste Date vor, gleich oder nach der zweiten Date liegt.

## Tiefes Eintauchen

Wenn Sie noch tiefer in das Vergleichen von zwei Daten in Elm eintauchen möchten, gibt es noch viele andere Methoden und Funktionen, die Sie verwenden können. Ein Beispiel dafür ist `Time.compareInUtc`, das die Zeit in UTC-Variante vergleicht, oder `Time.dayOfYear`, das den Tag im Jahr zurückgibt. Sie können auch benutzerdefinierte Funktionen schreiben, um spezifischere Vergleiche durchzuführen, wie zum Beispiel das Vergleichen von Daten mit unterschiedlichen Zeitformaten.

## Siehe auch

- [Elm Dokumentation - Time](https://elm-lang.org/docs/time)
- [Blog post - Vergleichen von zwei Daten in Elm](https://medium.com/@joeljosephjohnson/elm-101-comparing-dates-83cb35ed8055)
- [Github Gist - Vergleichen von zwei Daten in Elm](https://gist.github.com/ngvasa/f6411e4a3b7e8c72da9ce31a90727982)