---
title:    "C#: Schreiben auf den Standardfehler"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Das Schreiben auf den Standardfehler ist ein wichtiger Teil des Programmierens in C#. Es kann verwendet werden, um Fehlermeldungen oder Warnungen während der Laufzeit anzuzeigen und zu beheben. Dies ist besonders nützlich für die Fehlersuche und das Debuggen von Code.

## Wie geht man vor

Um auf den Standardfehler zu schreiben, können Sie die Methode `Console.Error.WriteLine()` verwenden. Dies ermöglicht es Ihnen, einen Text auf den Standardfehlerausgabestream zu schreiben, der vom Betriebssystem behandelt wird.

```C#
Console.Error.WriteLine("Dies ist ein Fehler!");
```

Dieses Beispiel würde die Nachricht "Dies ist ein Fehler!" auf den Standardfehlerausgabestream schreiben und vom Betriebssystem behandelt werden.

## Tiefergehende Informationen

Wenn Sie genauer verstehen möchten, wie das Schreiben auf den Standardfehler funktioniert, können Sie die Unterschiede zwischen `Console.Error.Write()` und `Console.Error.WriteLine()` untersuchen. Die erste Methode würde die Nachricht ohne eine neue Zeile an den Standardfehlerausgabestream schreiben, während die zweite Methode eine neue Zeile am Ende hinzufügen würde.

Sie können auch die Verwendung von `using System.Diagnostics;` in Betracht ziehen, um auf die `Trace` Klasse zuzugreifen, die zusätzliche Möglichkeiten zum Schreiben auf den Standardfehlerausgabestream bietet.

## Siehe auch

- [Console.Error.WriteLine-Methode (System)](https://docs.microsoft.com/de-de/dotnet/api/system.console.error.writeline)
- [Trace Class (System.Diagnostics)](https://docs.microsoft.com/de-de/dotnet/api/system.diagnostics.trace)
- [Schreiben auf die Standardausgabe und den Standardfehler im C#](https://www.tutorialspoint.com/writing-to-standard-output-and-standard-error-in-csharp)