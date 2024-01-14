---
title:    "C#: Schreiben in den Standardfehler"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Warum

Wenn du in der Welt des C# Programmierens unterwegs bist, wirst du früher oder später auf die sogenannte "Standardfehlerausgabe" (*standard error*) stoßen. Diese Ausgabe ist eine wichtige Methode, um Fehler in deinem Code zu erkennen und zu debuggen. In diesem Blogbeitrag werden wir uns genauer damit befassen, warum es wichtig ist, zur Standardfehlerausgabe zu schreiben und wie man dies effektiv tun kann.

## Wie man zur Standardfehlerausgabe schreibt

Um zur Standardfehlerausgabe zu schreiben, musst du zunächst die `Console` Klasse aus der `System` Namespace importieren. Dann kannst du die Methode `Error.WriteLine()` verwenden, um eine Nachricht zur Standardfehlerausgabe zu senden.

```C#
using System;

// ...

Console.Error.WriteLine("Dies ist eine Fehlermeldung.");
```

Die Methode `Error.WriteLine()` akzeptiert eine beliebige Zeichenkette als Argument und gibt sie auf der Standardfehlerausgabe aus. Du kannst der Methode auch mehrere Argumente übergeben, die dann jeweils mit einem Leerzeichen zwischen ihnen ausgegeben werden.

```C#
Console.Error.WriteLine("Ein Fehler trat bei der Berechnung von {0} auf. Fehlernummer: {1}", berechnung, fehlernummer);
```

Ein weiterer hilfreicher Tipp ist die Verwendung der `Error.SetOut()` Methode, um die Standardfehlerausgabe auf eine Datei oder einen Textwriter zu lenken. Dadurch kannst du Fehler in einer separaten Datei oder einem separaten Textbereich für eine einfachere Analyse speichern.

## Tiefer einsteigen

Nun, da du weißt, wie man zur Standardfehlerausgabe schreibt, lass uns einen tieferen Einblick in dieses Thema werfen. Im Grunde genommen ist die Standardfehlerausgabe eine spezielle Art der Konsolenausgabe, die dazu dient, Fehler und Ausnahmen in deinem Code anzuzeigen. Im Gegensatz dazu ist die Standardausgabe (*standard output*) für die reguläre Ausgabe zuständig, wie zum Beispiel das Anzeigen von Zeichenketten oder Werten.

Es ist wichtig, die Standardfehlerausgabe zu nutzen, da sie eine effektive Methode ist, um Probleme in deinem Programm zu identifizieren. Wenn du nur die Standardausgabe verwendest, kannst du möglicherweise wichtige Fehlermeldungen verpassen, da sie zusammen mit anderen Ausgaben angezeigt werden.

## Siehe auch

- [Microsoft Dokumentation: Console.Error Property](https://docs.microsoft.com/en-us/dotnet/api/system.console.error?view=netcore-3.1)
- [Tutorial: Fehlermeldung in C#](https://www.tutorialsteacher.com/csharp/csharp-console-application)
- [Verwenden von Standardfehlerausgaben in C#](https://exceptionnotfound.net/using-strings-in-standard-error-and-standard-output-in-c-sharp-net/)