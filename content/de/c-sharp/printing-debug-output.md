---
title:                "C#: Ausgabe von Debuginformationen drucken"
programming_language: "C#"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

Warum: Debugging-Output zu drucken kann ein nützliches Werkzeug sein, um Fehler in deinem C# Code zu finden und zu lösen.

Wie geht's: Um Debugging-Output in deinem Code zu drucken, kannst du die Methode "Console.WriteLine()" verwenden. Dies ermöglicht es dir, Text auf der Konsole auszugeben, um den Wert von Variablen oder andere relevante Informationen anzuzeigen.

```C#
int num1 = 5;
int num2 = 10;
Console.WriteLine("Der Wert von num1 ist: " + num1);
Console.WriteLine($"Der Wert von num2 ist: {num2}"); 
```

Ausgabe:
```
Der Wert von num1 ist: 5
Der Wert von num2 ist: 10
```

Tipp: Du kannst auch Kombinationen von Text und Variablen innerhalb der "Console.WriteLine()" Methode verwenden, um mehr Informationen auszugeben.

Tiefergehende Informationen: Wenn du noch tiefer in das Thema Debugging-Output eintauchen möchtest, gibt es einige zusätzliche Techniken, die du verwenden kannst. Zum Beispiel kannst du die Methode "Console.Clear()" verwenden, um die Konsole vor jedem Aufruf von "Console.WriteLine()" zu leeren. Dies verhindert, dass sich die Ausgabe überschneidet und macht das Lesen des Outputs einfacher.

Siehe auch:
- [Microsoft Dokumentation zu Console.WriteLine()](https://docs.microsoft.com/en-us/dotnet/api/system.console.writeline)
- [Tutorial zu Debugging in C#](https://www.tutorialspoint.com/csharp/csharp_debugging.htm)
- [Blog Artikel: 5 Tipps zum effektiven Debugging in C#](https://www.syncfusion.com/blogs/post/5-tips-to-make-your-csharp-debugging-more-effective.aspx)