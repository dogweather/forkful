---
title:                "C#: Großschreibung eines Strings"
programming_language: "C#"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich die Mühe machen, einen String in C# zu capitalisieren? Nun, es gibt viele Situationen, in denen man einen String in einer bestimmten Formatierung benötigt, sei es für die Benutzeroberfläche einer Anwendung oder für bestimmte Datenbank-Abfragen. Durch die Großschreibung kann man auch die Lesbarkeit und Verständlichkeit von Strings verbessern.

## Wie

Das Capitalizing eines Strings in C# ist relativ einfach. Man kann dafür entweder die char-Array Methode oder die String-Format Methode verwenden.

### Char-Array Methode

```
string myString = "Hallo Welt";
char[] charArray = myString.ToCharArray(); 
charArray[0] = char.ToUpper(charArray[0]);
myString = new string(charArray);
Console.WriteLine(myString); //Output: "Hallo Welt"
```

### String-Format Methode

```
string myString = "Hallo Welt";
myString = string.Format("{0}{1}", myString[0].ToString().ToUpper(), myString.Substring(1));
Console.WriteLine(myString); //Output: "Hallo Welt"
```

## Deep Dive

Bei genauerem Hinsehen lässt sich feststellen, dass der Code für das Capitalizing eines Strings manchmal auch von der Sprache der Anwendung abhängig sein kann. In manchen Sprachen unterscheiden sich die Groß- und Kleinschreibung von Buchstaben sehr stark und es kann komplexere Logik benötigt werden, um den String richtig zu capitalisieren. Auch innerhalb der verschiedenen .NET Frameworks gibt es Unterschiede, wie das Capitalizing funktioniert. Daher ist es wichtig, den Kontext zu berücksichtigen, in dem der String verwendet wird.

## Siehe auch

- Wie man einen String in C# trimmt: https://www.c-sharpcorner.com/article/how-to-trim-a-string-in-c-sharp/
- Die offizielle .NET Dokumentation zum String Capitalizing: https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/strings/capitalizing-a-string