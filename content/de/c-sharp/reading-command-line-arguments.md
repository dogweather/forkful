---
title:    "C#: Lesen von Befehlszeilenargumenten"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Haben Sie sich jemals gefragt, wie Sie Befehlszeilenargumente in Ihrem C# Programm lesen können? Möchten Sie eleganter auf Benutzereingaben reagieren? In diesem Blog-Beitrag erfahren Sie, warum das Lesen von Befehlszeilenargumenten nützlich sein kann.

## Wie es geht

Die Verwendung von Befehlszeilenargumenten in Ihrem C# Code ist einfacher als Sie denken. Zunächst müssen Sie die `Main`-Methode ändern, indem Sie die `args`-Parameter hinzufügen. Diese Parameter enthalten alle vom Benutzer eingegebenen Befehlszeilenargumente als Array von Zeichenfolgen. Hier ist ein Beispiel dafür, wie Sie die `Main`-Methode schreiben könnten:

```C#
static void Main(string[] args)
{
    // Code hier
}
```

Stellen Sie sich vor, Sie möchten, dass Ihr Programm eine Zahl als Befehlszeilenargument erhält und diese in eine `int`-Variable speichert. Hier ist, wie Sie das mit Befehlszeilenargumenten tun könnten:

```C#
static void Main(string[] args)
{
    // Überprüfen, ob mindestens ein Argument eingegeben wurde
    if (args.Length > 0)
    {
        // Das erste Argument als Zahl parsen und in einer Variable speichern
        int zahl = Int32.Parse(args[0]);
        // Ausgabe des Ergebnisses
        Console.WriteLine($"Die Zahl war {zahl}");
    }
    else
    {
        // Ausgabe einer Fehlermeldung, wenn kein Argument eingegeben wurde
        Console.WriteLine("Es wurde kein Befehlszeilenargument eingegeben.");
    }
}
```

Nun können Sie Ihr Programm ausführen und ihm eine Zahl als Argument übergeben, z.B. `dotnet ProgrammName.dll 42`. Die Ausgabe wäre `Die Zahl war 42`.

## Tiefer gehend

Sie können nicht nur Zahlen, sondern auch andere Datentypen als Befehlszeilenargumente übergeben, wie z.B. `string` oder `bool`. Außerdem können Sie mehrere Argumente in einer Zeile eingeben, indem Sie sie mit Leerzeichen trennen. In unserem Beispiel würde dies mit `dotnet ProgrammName.dll 42 true` funktionieren.

Es gibt auch Möglichkeiten, die Eingabe der Befehlszeilenargumente zu überprüfen und zu validieren, z.B. mit Regulären Ausdrücken oder Try-Methoden. Es lohnt sich also, sich mit den verschiedenen Optionen vertraut zu machen, um Ihre Befehlszeilenargumente effektiv zu nutzen.

## Siehe auch

- [Offizielle Dokumentation zu Befehlszeilenargumenten in C#](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)
- [Tutorial zum Lesen von Befehlszeilenargumenten in C#](https://www.codeproject.com/Articles/3111/Reading-Command-Line-Arguments)
- [Beispielcode für die Verwendung von Befehlszeilenargumenten in C#](https://www.dotnetperls.com/args)

Danke, dass Sie meinen Blog-Beitrag gelesen haben. Ich hoffe, Sie fühlen sich nun in der Lage, Befehlszeilenargumente in Ihren C# Programmen zu nutzen. Bis zum nächsten Mal!