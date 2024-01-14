---
title:                "C#: Teilstrings extrahieren"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

Warum: Oftmals müssen Entwicklerinnen und Entwickler in der Programmierung spezifische Teile von Texten oder Zeichenfolgen extrahieren. Dies kann zum Beispiel beim parsen von Daten oder beim Bearbeiten von Dateinamen nützlich sein.

Wie geht das?: Die C# Programmiersprache bietet die Möglichkeit, Substrings aus einem String-Objekt zu extrahieren. Hierfür gibt es verschiedene Methoden, die je nach Anforderung ausgewählt werden können. Im Folgenden findest du einige Beispiele mit Erklärungen.

```C#
// Beispiel 1: Substring mit Angabe von Startindex und Länge
string text = "Dies ist ein Beispieltext.";
string substring = text.Substring(8, 7); // Ergebnis: "ein Bei"

// Beispiel 2: Substring mit Angabe von Startindex
string result = text.Substring(17); // Ergebnis: "Beispieltext."
```

Es ist auch möglich, mit den Methoden `Substring()` und `IndexOf()` einen Teil eines Textes zwischen zwei bestimmten Zeichen zu extrahieren.

```C#
// Beispiel 3: Substring zwischen bestimmten Zeichen extrahieren
string sentence = "Ich mag Käse sehr gerne.";
int startIndex = sentence.IndexOf("Käse") + 5;
int length = sentence.IndexOf("sehr") - startIndex;
string result = sentence.Substring(startIndex, length); // Ergebnis: "sehr gerne"
```

Deep Dive: Die `Substring()` Methode akzeptiert sowohl eine Startposition als auch eine Länge als Parameter. Die Startposition kann entweder übergeben werden oder durch die `IndexOf()` Methode bestimmt werden. Zudem gibt es auch eine Überladung der Methode, die nur einen Parameter (die Startposition) benötigt und den Rest des Strings bis zum Ende extrahiert. Es ist wichtig zu beachten, dass die Indizes in C# bei 0 beginnen und dass die Länge immer kleiner oder gleich der tatsächlichen Länge des Textes sein muss.

Siehe auch: Weitere Informationen zu Substring-Extraktion in C# findest du in der offiziellen Dokumentation und in folgenden Artikeln:

- [String.Substring Method (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.string.substring)
- [Get a Substring (indeec Answers)](https://stackoverflow.com/questions/41971045/get-a-substring-between-two-characters-using-c-sharp)
- [How to extract a string in C# (The CodeHaven)](https://thecodehaven.com/2018/04/18/how-to-extract-a-string-in-c/)