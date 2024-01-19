---
title:                "Zeichen löschen, die einem Muster entsprechen"
html_title:           "C#: Zeichen löschen, die einem Muster entsprechen"
simple_title:         "Zeichen löschen, die einem Muster entsprechen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Löschung von Zeichen, die einem Muster entsprechen, bezieht sich darauf, bestimmte Zeichen aus einem String zu entfernen. Diese Operation ist nützlich, wenn Sie unerwünschte oder nicht benötigte Zeichen aus Ihrer Eingabe entfernen möchten.

## Wie man es macht:

In Haskell können wir die Bibliotheksfunktion `filter` verwenden, um diese Aufgabe zu erfüllen. Nachfolgend finden Sie ein einfaches Beispiel:

```Haskell
import Data.Char (isDigit)

deleteDigits :: String -> String
deleteDigits = filter (not . isDigit)

main = print (deleteDigits "Hello123World45")
```

Wenn Sie dieses Skript ausführen, erhalten Sie die Ausgabe:

```Haskell
"HelloWorld"
```

## Tiefere Einblicke

Die Funktion `filter` ist eine Standardbibliotheksfunktion, die in Haskell seit seiner ersten Veröffentlichung vorhanden ist. Sie nimmt eine Funktion und eine Liste als Eingabe und gibt eine Liste zurück, die nur die Elemente enthält, für die die Funktion `True` gibt.

Es gibt andere Methoden, um diese Aufgabe zu erfüllen, z.B. mit einer Schleife oder mit einer rekursiven Funktion, aber die `filter` Methode ist die einfachste und meist verwendete Methode in Haskell.

Die Implementierung von `filter` ist ziemlich einfach und effizient. Sie durchläuft die Liste einmal und fügt nur die Elemente in die neue Liste ein, die die Bedingung erfüllen, daher hat sie eine lineare Zeitkomplexität.

## Weitere Infos

Für weiterführende Informationen können Sie die Haskell-Dokumentation für die 'filter'-Funktion besuchen: http://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#v:filter

Und hier ist ein Link zu einem Beitrag über die Verwendung der `filter` Funktion in Haskell, der auch einige fortgeschrittene Nutzungsszenarien behandelt: https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/text

Hoffentlich hilft Ihnen dieser kurze Leitfaden beim effizienten Umgang mit Textbearbeitungsaufgaben in Haskell. Viel Spaß beim Codieren!