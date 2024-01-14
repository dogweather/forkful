---
title:    "Elm: Unterzeichenfolgen extrahieren"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Für das Verarbeiten von Texten in Elm gibt es oft die Notwendigkeit, bestimmte Teile eines Strings, auch Substrings genannt, zu extrahieren. Dies kann nützlich sein, um bestimmte Informationen aus einem längeren Text zu filtern oder um Text in kleinere Teile aufzuteilen. In diesem Beitrag werden wir uns genauer ansehen, wie man Substrings in Elm extrahieren kann.

## Wie man Substrings in Elm extrahiert

Es gibt verschiedene Möglichkeiten, Substrings in Elm zu extrahieren. Eine davon ist die Verwendung der Funktion `String.slice` aus dem Modul `String`. Diese Funktion nimmt als Parameter den Startindex und den Endindex des Substrings und gibt den entsprechenden Teil des Strings zurück. Zum Beispiel:

```elm
String.slice 3 6 "Elephant" == "eph"
```

Der Substring, der hier extrahiert wird, hat einen Startindex von 3 (das "e" in "Elephant") und einen Endindex von 6 (das "h" in "Elephant"). Beachte, dass der Endindex nicht im Substring enthalten ist, daher wird "h" nicht zurückgegeben.

Man kann auch negative Indices verwenden, um von hinten zu zählen. Zum Beispiel würde `String.slice -3 -1 "Elephant" == "an"` zurückgeben. 

Eine weitere Möglichkeit ist die Verwendung von regulären Ausdrücken mit der Funktion `Regex.find` aus dem Modul `Regex`. Diese Funktion nimmt als Parameter eine Regex und einen String und gibt eine Liste mit allen gefundenen Substrings zurück. Hier ist ein Beispiel:

```elm
Regex.find (Regex.regex "lo+") "Hello World" == [ "lo", "loo" ]
```

Dieser Code wird den Substring "lo" und "loo" in "Hello World" finden und in einer Liste zurückgeben. Beachte, dass reguläre Ausdrücke eine etwas fortgeschrittenere Technik sind, aber sehr nützlich, wenn man komplexe Suchmuster benötigt.

## Tiefergehende Information

Es gibt noch viele weitere Möglichkeiten, Substrings in Elm zu extrahieren. Zum Beispiel gibt es die Funktion `String.left` und `String.right`, die jeweils die ersten bzw. letzten n Zeichen eines Strings zurückgeben. Auch kann man den Startindex und Endindex in `String.slice` durch eine Funktion ersetzen, um dynamisch den zu extrahierenden Substring zu bestimmen.

Es ist wichtig, genau zu überlegen, welche Methode am besten für die jeweilige Aufgabe geeignet ist. Zum Beispiel ist es möglicherweise effizienter, `String.left` oder `String.right` zu verwenden, wenn man nur die ersten bzw. letzten n Zeichen eines Strings benötigt, anstatt die Funktion `String.slice` mit festen Indices zu nutzen.

## Siehe auch

- [Elm Dokumentation zu Substrings](https://package.elm-lang.org/packages/elm/core/latest/String#slice)
- [Elm Dokumentation zu regulären Ausdrücken](https://package.elm-lang.org/packages/elm/regex/latest/Regex)
- [Elm Dokumentation zu String Funktionen](https://package.elm-lang.org/packages/elm/core/latest/String)