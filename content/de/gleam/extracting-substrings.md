---
title:                "Gleam: Unterstrings extrahieren"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Extrahieren von Teilstrings ist eine wichtige Aufgabe in der Programmierung, besonders wenn es um die Manipulation von Texten geht. Es ermöglicht es uns, bestimmte Teile eines Textes zu isolieren und sie für weitere Verarbeitungsschritte zu verwenden. In diesem Blog-Beitrag werden wir uns ansehen, wie man in der funktionalen Programmiersprache Gleam Teilstrings extrahieren kann.

## Wie Geht Das

Um Teilstrings in Gleam zu extrahieren, können wir die `substring` Funktion verwenden. Diese Funktion erwartet als Argumente den Text, aus dem der Teilstring extrahiert werden soll, sowie die Start- und Endposition des Teilstrings.

```Gleam
let text = "Gleam ist eine funktionale, typsichere Programmiersprache"
let substring = substring(text, 16, 27)
```

In diesem Beispiel extrahieren wir den Teilstring "funktionale" aus dem ursprünglichen Text und speichern ihn in der Variable `substring`. Die Zählung der Positionen beginnt bei 0, daher ist die Startposition 16 und die Endposition 27.

Wenn wir das Programm ausführen und `substring` ausgeben, erhalten wir das gewünschte Ergebnis:

```Gleam
io.println(substring)
// funktionale
```

## Ein Tiefer Einblick

Die `substring` Funktion in Gleam ist sehr flexibel und bietet verschiedene Möglichkeiten, Teilstrings zu extrahieren. Wir können beispielsweise auch angeben, ab welcher Position der Teilstring beginnen soll und den Rest des Textes extrahieren:

```Gleam
substring(text, 16) // gibt "funktionale, typsichere Programmiersprache" zurück
```

Außerdem können wir negative Zahlen verwenden, um die Positionen von hinten zu zählen. Wenn wir beispielsweise `-3` als Endposition angeben, extrahiert die Funktion die letzten drei Buchstaben des Textes.

```Gleam
substring(text, -3) // gibt "age" zurück
```

Es ist auch möglich, den Begriff "länge" als Endposition anzugeben, um den Teilstring bis zum Ende des Textes zu extrahieren.

```Gleam
substring(text, 16, "länge") // gibt "funktionale, typsichere Programmiersprache" zurück
```

## Siehe Auch

- Dokumentation zur `substring` Funktion in Gleam: https://gleam.run/documentation/stdlib/string#substring
- Einführung in Gleam: https://gleam.run/getting-started/introduction.html
- Tutorial zur funktionalen Programmierung: https://www.tutorialspoint.com/functional_programming/index.htm