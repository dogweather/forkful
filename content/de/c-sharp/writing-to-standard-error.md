---
title:                "Schreiben auf den Standardfehler"
html_title:           "C#: Schreiben auf den Standardfehler"
simple_title:         "Schreiben auf den Standardfehler"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Warum

Schreiben an den Standardfehler (Standard Error) ist eine wichtige Fähigkeit für jeden C# Programmierer. Durch das Schreiben an den Standardfehler können Fehler und Warnungen im Programm angezeigt werden, was es einfacher macht, Fehler bei der Ausführung zu beheben.

# Wie geht man vor?

```C#
Console.Error.WriteLine("This is an error message.");
```

Der obige Code gibt die Fehlermeldung "This is an error message." auf der Konsole aus, da wir die `Error` Eigenschaft des `Console` Objekts verwenden. Hierbei ist es wichtig, das `WriteLine` anstelle von `Write` zu verwenden, da dies sicherstellt, dass die Fehlermeldung in einer neuen Zeile ausgegeben wird.

```C#
try
{
  // Code, der möglicherweise einen Fehler auslöst
}
catch (Exception ex)
{
  Console.Error.WriteLine("Error: " + ex.Message);
}
```

In diesem Beispiel wird die Fehlermeldung sowohl im `catch` Block als auch im `try` Block ausgegeben. Dies ist hilfreich, um zu überprüfen, ob der Code im `try` Block erfolgreich ausgeführt wurde oder ob ein Fehler aufgetreten ist.

# Tiefer geht's

Das Schreiben an den Standardfehler ist besonders hilfreich bei der Entwicklung von komplexen Programmen oder beim Debugging. Durch die Verwendung von `Console.Error` können wir gezielt Fehler oder Warnungen ausgeben, ohne das normale Programmverhalten zu beeinträchtigen. Es ist auch möglich, den Standardfehler in eine Datei umzuleiten, um spätere Fehleranalysen zu ermöglichen.

# Siehe auch

- Dokumentation zur `Console.Error` Eigenschaft (https://docs.microsoft.com/dotnet/api/system.console.error)
- Artikel zur Fehlerbehandlung in C# (https://www.c-sharpcorner.com/article/exception-handling-in-c-sharp/)