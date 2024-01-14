---
title:    "C#: Das Lesen von Befehlszeilenargumenten"
keywords: ["C#"]
---

{{< edit_this_page >}}

## Warum

Wenn Sie in der Welt des Programmierens unterwegs sind, haben Sie wahrscheinlich schon von Command Line Arguments gehört. Aber warum sind sie wichtig und warum sollten Sie sich damit befassen? Nun, Command Line Arguments sind ein wichtiger Teil beim Erstellen von interaktiven Tools, Scripting-Tools und anderen Anwendungen, die Benutzerinteraktion erfordern. Sie ermöglichen dem Benutzer, schnell und einfach Einstellungen und Parameter anzugeben, die das Verhalten der Anwendung beeinflussen.

## Wie geht es

Das Lesen von Befehlszeilenargumenten in C# ist relativ einfach. Nehmen wir zum Beispiel an, Sie möchten einen einfachen Taschenrechner erstellen, der zwei Zahlen addieren kann. Sie möchten, dass der Benutzer die Zahlen über die Befehlszeile eingibt. Hier ist ein Beispiel für die Umsetzung dieses Szenarios:

```C#
using System;

class Taschenrechner 
{
    static void Main(string[] args) 
	{
        // Überprüfen, ob zwei Argumente eingegeben wurden
		if (args.Length == 2) 
		{
            // Konvertieren der Argumente in Integer
			int x = Convert.ToInt32(args[0]);
			int y = Convert.ToInt32(args[1]);

            // Berechnen und Ausgabe des Ergebnisses
			Console.WriteLine($"Summe: {x} + {y} = {x + y}");
		} 
		else 
		{
            // Fehlermeldung, wenn nicht genügend Argumente eingegeben werden
			Console.WriteLine("Bitte geben Sie zwei Zahlen als Argumente ein.");
		}
	}
}
```

Wenn ein Benutzer nun den Befehl `dotnet taschenrechner.dll 3 5` ausführt, wird das Ergebnis "Summe: 3 + 5 = 8" ausgegeben.

## Tiefer gehend

Es gibt noch viele weitere Möglichkeiten, wie Sie Command Line Arguments in Ihren C#-Anwendungen nutzen können. Sie können beispielsweise verschiedene Datentypen als Argumente akzeptieren, Standardwerte für die Argumente festlegen oder sogar mehrere Parameter für eine bestimmte Einstellung zulassen. Es ist auch möglich, Argumente über Optionen oder Flags zu verarbeiten, die mit verschiedenen Bibliotheken wie "CommandLineUtils" oder "PowerArgs" implementiert werden können.

Um noch weiter in die Materie einzutauchen, empfehlen wir Ihnen, sich mit der Klasse `System.Environment.CommandLine` vertraut zu machen, die Ihnen noch mehr Informationen über die Befehlszeile und die eingegebenen Argumente liefert.

## Siehe auch

- [Microsoft Dokumentation über Command Line Arguments](https://docs.microsoft.com/en-us/dotnet/core/extensions/command-line-args)
- [CommandLineUtils Bibliothek](https://www.nuget.org/packages/CommandLineUtils/)
- [PowerArgs Bibliothek](https://www.nuget.org/packages/PowerArgs/)