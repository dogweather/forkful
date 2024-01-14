---
title:                "C#: Schreiben auf die Standardfehlerausgabe"
programming_language: "C#"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum?

In diesem Blogbeitrag werden wir untersuchen, warum es wichtig ist, in C# auf die Standardfehlerausgabe zu schreiben. Die Standardfehlerausgabe, auch als stderr bezeichnet, ist ein wichtiger Teil jeder Programmiersprache und kann bei der Fehlersuche und Debugging-Prozessen sehr hilfreich sein.

## Wie geht das?

Um in C# auf die Standardfehlerausgabe zu schreiben, können wir die Methode `Console.Error.WriteLine()` verwenden. Diese Methode gibt eine Zeichenfolge auf die Standardfehlerausgabe aus, gefolgt von einem Zeilenumbruch. Hier ist ein Beispiel:

```C#
Console.Error.WriteLine("Schwerwiegender Fehler aufgetreten!");
```

Dieser Code gibt die Zeichenfolge "Schwerwiegender Fehler aufgetreten!" auf die Standardfehlerausgabe aus. Die Ausgabe könnte wie folgt aussehen:

```
Schwerwiegender Fehler aufgetreten!
```

Um weitere Zeichenfolgen auf die Standardfehlerausgabe auszugeben, können wir die Methode `Console.Error.Write()` verwenden. Diese Methode gibt eine Zeichenfolge ohne Zeilenumbruch aus. Hier ist ein Beispiel:

```C#
Console.Error.Write("Details:");
Console.Error.Write(" Keine Verbindung zum Server.");
```

Dieser Code gibt die Zeichenfolge "Details: Keine Verbindung zum Server." auf die Standardfehlerausgabe aus. Die Ausgabe könnte wie folgt aussehen:

```
Details: Keine Verbindung zum Server.
```

## Tieferes Eintauchen

Es gibt verschiedene Situationen, in denen das Schreiben auf die Standardfehlerausgabe hilfreich sein kann. Zum Beispiel, wenn ein Programm viele Informationsausgaben hat, aber man möchte nur die Fehlermeldungen sehen, kann man die Standardfehlerausgabe nutzen. Auch beim Debugging kann es nützlich sein, auf die Standardfehlerausgabe zu schreiben, um gezielt nach bestimmten Fehlermeldungen zu suchen.

Eine andere wichtige Anwendung für die Standardfehlerausgabe ist die Verwendung von Error-Log-Dateien. Diese Dateien enthalten alle Fehlermeldungen, die während der Programmausführung aufgetreten sind und können später zur Fehlersuche verwendet werden.

Es ist auch erwähnenswert, dass die Standardfehlerausgabe von den meisten Konsolenanwendungen standardmäßig verwendet wird, um Fehlermeldungen auszugeben. Daher ist es wichtig, beim Programmieren von Konsolenanwendungen auf die Standardfehlerausgabe zu achten.

## Siehe auch

- [Console-Klasse (System.Console) in der Microsoft-Dokumentation](https://docs.microsoft.com/de-de/dotnet/api/system.console?view=netcore-3.1)
- [Tutorial: Fehlerbehandlung und Debugging in C#](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/exceptions/)
- [Artikel über die Verwendung der Standardfehlerausgabe in C#](https://www.c-sharpcorner.com/article/understanding-standard-error-in-c-sharp/)