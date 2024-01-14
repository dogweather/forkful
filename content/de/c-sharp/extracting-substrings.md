---
title:    "C#: Teilstrings extrahieren"
keywords: ["C#"]
---

{{< edit_this_page >}}

# Warum substrings extrahieren?

Beim Programmieren in C# läuft man oft in Situationen, in denen man bestimmte Teile eines Strings extrahieren muss, zum Beispiel um einen Nutzernamen aus einer E-Mail Adresse zu isolieren oder um einen Datensatz aus einer großen Textdatei zu finden. In solchen Fällen ist es unerlässlich, substrings zu extrahieren.

## Wie man substrings extrahiert

Substring-Extraktion in C# erfolgt mithilfe der `Substring()` Methode. Hier ist ein Beispiel, wie man den Benutzernamen aus einer E-Mail Adresse extrahiert:

```C#
string email = "example123@email.com";
string username = email.Substring(0, email.IndexOf("@"));
Console.WriteLine(username);
```

Die Ausgabe würde "example123" sein, da der Substring von Index 0 bis zum ersten Vorkommen des "@"-Zeichens extrahiert wird.

Ein weiteres Beispiel, wie man einen Teil eines Strings aus einer längeren Textdatei extrahiert:

``` C#
string input = "Welcome to this tutorial! I hope it helps you in your coding journey.";
string extracted = input.Substring(11, 21);
Console.WriteLine(extracted);
```

Die Ausgabe würde "tutorial! I hope" sein, da der Substring ab dem 11. Zeichen bis zum 21. Zeichen extrahiert wird.

## Tiefergehende Informationen über Substring-Extraktion

Die `Substring()` Methode nimmt zwei Argumente: den Startindex und die Anzahl der zu extrahierenden Zeichen. Es ist auch möglich, nur den Startindex anzugeben und die restlichen Zeichen bis zum Ende des Strings automatisch zu extrahieren.

Es ist wichtig zu beachten, dass der Index in C# bei 0 beginnt, daher ist das erste Zeichen des Strings mit Index 0 gekennzeichnet. Wenn man nur einen Startindex angibt, wird der String ab diesem Index bis zum Ende extrahiert.

## Siehe auch

- [MSDN Dokumentation zur `Substring()` Methode](https://docs.microsoft.com/de-de/dotnet/api/system.string.substring)
- [Tutorial zur String Manipulation in C#](https://www.tutorialspoint.com/csharp/csharp_strings.htm)
- [Video-Tutorial zur Substring-Extraktion in C#](https://www.youtube.com/watch?v=QBDT7an789o)