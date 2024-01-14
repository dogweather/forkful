---
title:    "C#: Debug-Ausgabe drucken"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/printing-debug-output.md"
---

{{< edit_this_page >}}

# Warum

Das Drucken von Debug Output ist ein nützliches Tool für Entwickler, um Fehler in ihrem Code zu finden und zu beheben. Durch das Anzeigen von Variablenwerten und Nachrichten während der Ausführung wird es einfacher, die zugrunde liegenden Probleme zu identifizieren.

# Wie geht man vor

Um Debug Output in C# zu drucken, können Sie die `Console.WriteLine()` Methode verwenden, gefolgt von der zu druckenden Nachricht oder Variable. Hier ist ein Codebeispiel:

```C#
int myNumber = 10;
Console.WriteLine("Der aktuelle Wert von myNumber ist: " + myNumber);
```

Die Ausgabe dieses Codes würde folgendermaßen aussehen:

```
Der aktuelle Wert von myNumber ist: 10
```

Sie können auch Debug Output mit anderen nützlichen Methoden wie `Console.Write()` oder `Debug.WriteLine()` drucken. Diese Methoden bieten zusätzliche Funktionen wie z.B. das Formatieren von Strings oder das Einfügen von Variablenwerten in die Nachricht.

# Tiefere Einblicke

Beim Drucken von Debug Output ist es wichtig, die Ausgabe sorgfältig zu planen. Vermeiden Sie zu viele Ausgaben, die zu einem unübersichtlichen Output führen könnten. Stattdessen sollten Sie gezielt wichtige Variablen und Nachrichten auswählen, um Probleme besser zu verstehen.

Eine weitere nützliche Technik ist die Verwendung von bedingten Anweisungen, um Debug Output je nach Debug-Level zu drucken. Dadurch können Sie während der Entwicklung alle Nachrichten sehen, während in einer Produktionsumgebung nur wichtige Fehlermeldungen ausgegeben werden.

# Siehe auch

- [Guide für das Debugging in C#](https://docs.microsoft.com/de-de/visualstudio/debugger/debugger-feature-tour?view=vs-2019)
- [10 Tipps für effektives Debugging](https://raygun.com/blog/debugging-tips/)
- [Weitere Debugging Tools für C#](https://raygun.com/blog/top-c-sharp-debugging-tools/)