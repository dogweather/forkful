---
title:                "Ausgabe für die Fehlerbehebung drucken"
html_title:           "C#: Ausgabe für die Fehlerbehebung drucken"
simple_title:         "Ausgabe für die Fehlerbehebung drucken"
programming_language: "C#"
category:             "C#"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Was & Warum?
Bevor wir uns mit dem schönen Thema des Debuggens beschäftigen, gibt es einen wichtigen Vorsatz, den du dir merken solltest: Debugging ist der Prozess des "Zeitverschwendens". Ja, du hast richtig gehört. Es ist etwas, das wir Programmierer tun, um Probleme in unserem Code zu finden und zu beheben. Wir tun dies, weil wir wollen, dass unser Code ohne Fehler läuft und unsere Programme reibungslos funktionieren. Aber warte, gibt es nicht bessere Möglichkeiten, Probleme im Code zu lösen? Natürlich gibt es sie, aber Debugging ist immer noch ein wichtiger Teil des Programmierens und hilft uns, bessere Entwickler zu werden.

## Wie geht's:
Um Debugging in C# durchzuführen, gibt es unterschiedliche Ansätze. Der einfachste Weg ist die Verwendung von Debug.WriteLine(), um Text auf der Konsole auszugeben. Dieser Text wird nur angezeigt, wenn das Programm im Debug-Modus läuft. Zum Beispiel:

```C#
// Hier wird der Text "Hallo Welt!" in der Konsole ausgegeben.
Debug.WriteLine("Hallo Welt!");
```

Wenn du jedoch mehr Kontrolle über den Debug-Output haben möchtest, kannst du auch die Klasse System.Diagnostics.Debug verwenden. Mit dieser Klasse kannst du nicht nur Text ausgeben, sondern auch Variablenwerte überprüfen und sogar Breakpoints setzen, um den Ablauf deines Codes zu untersuchen.

```C#
// Hier wird eine Variable mit dem Wert 5 ausgegeben.
int zahl = 5;
Debug.Print(zahl);

// Hier wird ein Breakpoint gesetzt, um den Ablauf des Codes anzuhalten.
// Du kannst dann Schritt für Schritt durch deinen Code gehen und überprüfen, welche Werte deine Variablen haben.
Debug.WriteLine("Programm startet...");

// Dein Code hier

Debug.WriteLine("Programm beendet...");
```

## Tief einsteigen:
Debugging existiert schon seit den Anfängen des Programmierens. Früher benutzten Programmierer spezielle Hardware, um Bugs in ihren Programmen zu finden und zu beheben. Heutzutage haben wir viel fortgeschrittenere Tools und Methoden, aber Debugging bleibt ein wichtiger Bestandteil des Programmierens.

Neben der Verwendung von Debugging-Tools gibt es auch alternative Techniken, um Probleme im Code zu finden, wie zum Beispiel das Schreiben von Unit-Tests oder die Verwendung von Codeüberprüfungen durch andere Entwickler. All diese Methoden können zusammenarbeiten, um unseren Code fehlerfrei zu machen.

Wenn es um die Implementierung von Debugging in C# geht, ist es wichtig zu beachten, dass Debugging-Code normalerweise nur im Debug-Modus ausgeführt wird. Wenn dein Code in einer Produktionsumgebung läuft, werden Debug-Anweisungen einfach ignoriert. Dies ermöglicht es uns, Debugging in unserem Code zu haben, ohne dabei die Performance unseres Programms zu beeinträchtigen.

## Siehe auch:
Wenn du tiefer in die Welt des Debuggens eintauchen möchtest, gibt es viele nützliche Ressourcen, die du nutzen kannst. Hier sind einige, die dir den Einstieg erleichtern werden:

- [Debugging in C# mit Visual Studio](https://docs.microsoft.com/en-us/visualstudio/debugger/)- eine offizielle Dokumentation von Microsoft, die alles erklärt, was man über das Debuggen in C# wissen muss.
- [C# Debugging Tutorial](https://www.tutorialsteacher.com/csharp/csharp-debugging)- ein ausführliches Tutorial, das dir Schritt für Schritt zeigt, wie du dein C#-Programm debuggen kannst.
- [Debugging Tips and Tricks](https://stackify.com/debugging-tips-tricks/) - eine Sammlung von nützlichen Tipps und Tricks zum Debuggen in C#.

Also zögere nicht, Debugging in deinem Code zu verwenden. Es mag für viele wie eine Zeitverschwendung erscheinen, aber es ist besser, Probleme in deinem Code aufzudecken und zu beheben, bevor sie zu größeren Problemen führen. Happy debugging!