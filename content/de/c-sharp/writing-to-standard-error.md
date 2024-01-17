---
title:                "Schreiben auf Standardfehler"
html_title:           "C#: Schreiben auf Standardfehler"
simple_title:         "Schreiben auf Standardfehler"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Schreiben auf die Standardfehlerausgabe ist eine Möglichkeit für Programmierer, Fehlermeldungen oder andere wichtige Informationen während der Ausführung ihres Codes anzuzeigen. Es ist hilfreich, um Fehler zu identifizieren und zu beheben oder um den Fortschritt des Programms zu verfolgen.

## How to:
C# stellt uns verschiedene Methoden zur Verfügung, um auf die Standardfehlerausgabe zu schreiben. Hier sind zwei Beispiele:

```C#
// Beispiel 1: Verwendung von Console.Error.WriteLine()
Console.Error.WriteLine("Dies ist eine Fehlermeldung!");

/* Beispiel 2: Verwendung von Console.SetError()
* Wir können auch die Standardfehlerausgabe manuell ändern und eine Datei als Ausgabeziel festlegen.
*/
var errorFile = new FileStream("error.log", FileMode.Create);
Console.SetError(errorFile);
Console.WriteLine("Dieser Text wird auf die Datei 'error.log' geschrieben.");
```

Die Ausgabe für beide Beispiele sieht folgendermaßen aus:

```C#
Dies ist eine Fehlermeldung!
Dieser Text wird auf die Datei 'error.log' geschrieben.
```

## Deep Dive
Das Konzept des Schreibens auf die Standardfehlerausgabe existiert schon lange und wurde bereits in frühen Programmiersprachen wie C und Unix eingeführt. Es ist auch wichtig festzuhalten, dass die Standardfehlerausgabe von der Standardausgabe getrennt ist und somit unabhängig voneinander verwendet werden können.

Als Alternative kann auch die Standardausgabe verwendet werden, um Fehlermeldungen anzuzeigen. Allerdings ist dies nicht empfehlenswert, da die Standardausgabe für normale Programminteraktionen vorgesehen ist und somit Fehlermeldungen untergehen können.

Die Implementierung des Schreibens auf die Standardfehlerausgabe ist einfach und intuitiv, da es in der Standardbibliothek von C# bereits Funktionen dafür gibt. Es ist jedoch wichtig zu beachten, dass die Ausgabe auf die Standardfehlerausgabe nicht im Debug-Modus angezeigt wird. Stattdessen werden sie während des Kompilierens in die Fehlerliste geschrieben.

## See Also
Mehr Informationen zu den Funktionen zum Schreiben auf die Standardfehlerausgabe: https://docs.microsoft.com/dotnet/api/system.console.seterror?view=net-5.0

Weitere Informationen zur Standardfehlerausgabe: https://docs.microsoft.com/dotnet/api/system.console.error?view=net-5.0