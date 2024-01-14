---
title:    "C#: Unterstrings extrahieren"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Warum

 Das Extrahieren von Teilzeichenfolgen oder Substrings aus einem größeren String ist eine häufige Aufgabe bei der Programmierung. Zum Beispiel kann es notwendig sein, bestimmte Informationen aus einer längeren Textnachricht oder E-Mail zu isolieren. Die Verwendung von Substrings kann auch bei der Validierung von Benutzereingaben nützlich sein, um sicherzustellen, dass die erforderlichen Informationen in einer bestimmten Reihenfolge vorliegen.

# Wie

Das Extrahieren von Substrings ist in C# einfach und erfordert nur wenige Codezeilen. Zunächst muss der String, aus dem der Substring extrahiert werden soll, in einer Variablen gespeichert werden. Dann können wir die Methode "Substring" auf diese Variable anwenden, um den gewünschten Teil des Strings zu extrahieren.

```
// Beispielcode zur Extrahierung eines Substrings

string text = "Hallo, mein Name ist Max Mustermann.";
string substring = text.Substring(15, 11); // Das erste Argument ist der Startindex, das zweite Argument die Länge des Substrings.
Console.WriteLine(substring);
```

**Ausgabe:** Max Mustermann.

In diesem Beispiel haben wir einen Substring mit dem Namen "substring" erstellt, der aus dem ursprünglichen String "text" extrahiert wurde. Der Startindex "15" gibt an, an welcher Stelle der Extraction beginnen soll, und die Länge des Substrings "11" gibt an, wie viele Zeichen extrahiert werden sollen.

Um es noch etwas komplexer zu gestalten, können wir auch die Indexof-Methode verwenden, um den Startindex des zu extrahierenden Substrings zu finden.

```
// Beispielcode zur Verwendung der Indexof-Methode für die Extraktion eines Substrings

string text = "Ich lebe in Deutschland.";
int firstIndex = text.Indexof("Deutschland"); // Indexof gibt den Startindex des ersten Vorkommens des gesuchten Substrings zurück.
Console.WriteLine(text.Substring(firstIndex));
```

**Ausgabe:** Deutschland.

# Deep Dive

Bei der Verwendung von Substrings ist es wichtig zu beachten, dass in C# die Indizierung bei "0" beginnt. Das bedeutet, dass der erste Buchstabe eines Strings den Index "0" hat. Wenn wir also einen Substring aus dem Wort "Computer" extrahieren möchten, wäre der Index für den Buchstaben "C" "0" und nicht "1".

Eine weitere Sache, die man beachten sollte, ist, dass die Länge des extrahierten Substrings nicht größer sein darf als die Länge des ursprünglichen Strings, da sonst ein Fehler auftritt. Wenn also versucht wird, einen Substring mit einer Länge von 10 aus einem String mit einer Länge von 8 zu extrahieren, wird ein Fehler gemeldet.

# Siehe auch

- [Microsoft-Dokumentation zur String.Substring-Methode auf Deutsch](https://docs.microsoft.com/de-de/dotnet/api/system.string.substring)
- [Tutorial zur Verwendung von Substrings in C# (Englisch)](https://www.c-sharpcorner.com/article/using-substring-method-in-c-sharp-language/)