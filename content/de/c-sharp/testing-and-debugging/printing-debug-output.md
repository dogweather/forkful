---
date: 2024-01-20 17:52:25.551930-07:00
description: "Beim Debuggen von C# Programmen ist das Ausgeben von Zwischenergebnissen\
  \ und Fehlersuchinformationen essenziell. Es hilft Entwicklerinnen und Entwicklern,\u2026"
lastmod: '2024-03-13T22:44:53.890748-06:00'
model: gpt-4-1106-preview
summary: Beim Debuggen von C# Programmen ist das Ausgeben von Zwischenergebnissen
  und Fehlersuchinformationen essenziell.
title: Debug-Ausgaben drucken
weight: 33
---

## Was & Warum?
Beim Debuggen von C# Programmen ist das Ausgeben von Zwischenergebnissen und Fehlersuchinformationen essenziell. Es hilft Entwicklerinnen und Entwicklern, den Programmfluss zu verfolgen und Fehler schneller zu finden.

## How to:
Einfache Ausgabe auf der Konsole mit `Console.WriteLine`:

```C#
Console.WriteLine("Debug-Info: Variable x hat den Wert " + x);
```

Output:
```
Debug-Info: Variable x hat den Wert 42
```

Mit `System.Diagnostics.Debug` in einer Debug-Build:

```C#
Debug.WriteLine("Hier steht etwas Wichtiges.");
```

Sichtbar im Output-Fenster deiner Entwicklungsumgebung, aber nicht in der finalen Anwendung.

## Deep Dive
Die Verwendung von `Console.WriteLine` reicht für viele Debug-Zwecke aus, kommt aber ursprünglich aus der Welt der Konsolenanwendungen. Bei GUI-Anwendungen oder Diensten ist dies möglicherweise nicht ideal. `System.Diagnostics.Debug` dagegen schreibt Nachrichten in das Output-Fenster deiner Entwicklungsumgebung, so z.B. Visual Studio.

Eine Alternative bietet `Trace`, ähnlich zu `Debug`, erlaubt aber weitere Flexibilität durch Listener, die Nachrichten in verschiedenen Ausgaben darstellen können.

Beim Einsatz von Logging-Frameworks wie log4net oder NLog können programmatisch viel mehr Informationen, wie Zeitstempel oder Dateinamen, ohne zusätzlichen Codeaufwand geschrieben werden.

Schließlich gibt es noch die bedingte Kompilierung, wodurch Code nur in Debug-Builds ausgeführt wird:

```C#
#if DEBUG
Console.WriteLine("Diese Zeile erscheint nur im Debug-Modus");
#endif
```

## See Also
- Microsoft Dokumentation zu `System.Diagnostics`: [docs.microsoft.com](https://docs.microsoft.com/de-de/dotnet/api/system.diagnostics?view=net-6.0)
- Tutorial zu log4net: [https://logging.apache.org/log4net/](https://logging.apache.org/log4net/)
- Tutorial zu NLog: [https://nlog-project.org/](https://nlog-project.org/)
- Microsoft Dokumentation zu bedingter Kompilierung: [docs.microsoft.com](https://docs.microsoft.com/de-de/dotnet/csharp/language-reference/preprocessor-directives/preprocessor-if)
