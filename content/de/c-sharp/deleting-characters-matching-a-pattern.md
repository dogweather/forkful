---
title:                "C#: Entfernen von Zeichen, die einem Muster entsprechen."
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Warum

Es gibt verschiedene Gründe, warum man als Programmierer*in Charaktere löschen möchte, die einem bestimmten Muster entsprechen. Zum Beispiel kann es sein, dass man Daten bereinigen muss, bevor man sie weiterverarbeiten kann. Oder man möchte unerwünschte Zeichen aus einer Benutzereingabe entfernen, um die Sicherheit der Anwendung zu verbessern. In diesem Blog-Beitrag werde ich zeigen, wie man mithilfe von C# Charaktere löschen kann, die einem bestimmten Muster entsprechen.

# Wie geht das?

Um Charaktere zu löschen, die einem bestimmten Muster entsprechen, können wir die Methode `Remove()` aus der Klasse `String` verwenden. Diese Methode gibt eine neue Zeichenfolge zurück, in der alle Vorkommen des angegebenen Musters entfernt wurden. Hier ist ein Beispielcode, der alle Zahlen aus einer Zeichenfolge entfernt:

```C#
string input = "Abc123Def456";
string output = input.Remove("123");

Console.WriteLine(output); // Ausgabe: "AbcDef"
```

In diesem Beispiel wird die Methode `Remove()` auf der Eingabezeichenfolge `input` angewendet, wobei das Muster "123" als Parameter übergeben wird. Die neue Zeichenfolge `output` wird dann ohne alle Vorkommen von "123" sein.

# Tiefergehende Informationen

Man kann auch angeben, an welcher Position in der Eingabezeichenfolge das Muster beginnen soll, indem man einen zusätzlichen Parameter mit der Startposition angibt:

```C#
string input = "Abc123Def456";
string output = input.Remove("123", 3); // Startposition 3

Console.WriteLine(output); // Ausgabe: "AbcDef456"
```

In diesem Beispiel wird das Muster "123" erst ab der dritten Position in der Zeichenfolge gelöscht. Die Startposition wird dabei als zweites Argument in der Methode `Remove()` übergeben (beginnend bei 0 für das erste Zeichen).

Außerdem gibt es auch die Möglichkeit, anzugeben, wie viele Zeichen vom Muster gelöscht werden sollen, indem man einen weiteren Parameter mit der Länge angibt:

```C#
string input = "Abc123Def456";
string output = input.Remove("123", 3, 2); // Startposition 3, Länge 2

Console.WriteLine(output); // Ausgabe: "AbcDef456"
```

In diesem Beispiel wird das Muster "123" ab der dritten Position in der Zeichenfolge gelöscht und dabei nur die ersten beiden Zeichen des Musters entfernt.

# Siehe auch

- [MSDN Dokumentation zu der Methode `Remove()`](https://docs.microsoft.com/en-us/dotnet/api/system.string.remove?view=net-5.0)
- [Weitere Möglichkeiten, Zeichenfolgen in C# zu bearbeiten](https://www.c-sharpcorner.com/UploadFile/mahesh/string-manipulation-in-C-Sharp/#:~:text=Remove()%20method%3A-,The%20Remove()%20method,StartingIndex%2C%20int%20MaxLength)%20method)