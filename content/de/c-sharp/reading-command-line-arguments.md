---
title:                "Das Lesen von Befehlszeilen-Argumenten"
html_title:           "C#: Das Lesen von Befehlszeilen-Argumenten"
simple_title:         "Das Lesen von Befehlszeilen-Argumenten"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum
Es gibt verschiedene Gründe, warum man sich mit der Verwendung von Befehlszeilenargumenten in C# auseinandersetzen sollte. Vielleicht möchtest du eine Anwendung erstellen, die von der Kommandozeile aus gesteuert werden kann, oder du möchtest die Flexibilität haben, deine Anwendung durch das Übergeben von Argumenten anzupassen. Egal aus welchem Grund, die Kenntnis darüber, wie man Befehlszeilenargumente liest, ist eine nützliche Fähigkeit für jeden C#-Programmierer.

## Wie geht's

Um Befehlszeilenargumente in C# zu lesen, gibt es einige Schritte zu beachten. Zunächst musst du die `Main`-Methode in deinem Programm anpassen, damit diese Argumente akzeptiert. Hier ist ein Beispiel, wie das aussehen könnte:

```C#
static void Main(string[] args)
{
    // Hier ist der Code, der ausgeführt wird, wenn keine Argumente übergeben werden. 
    
    if(args.Length > 0)
    {
        // Hier kannst du die übergebenen Argumente nutzen.
    }
}
```

Die `Main`-Methode erwartet ein Array von Strings als Parameter, in diesem Fall `args`, in dem alle übergebenen Argumente gespeichert werden. Durch die Bedingung `args.Length > 0` prüfen wir, ob überhaupt Argumente übergeben wurden. Wenn dies der Fall ist, können wir sie im nächsten Schritt nutzen.

Hier ist ein Beispiel, wie man die übergebenen Argumente ausgibt:

```C#
// Annahme: Der erste übergebene Argument ist ein Name.
Console.WriteLine("Hallo " + args[0] + "!");
```

In diesem Beispiel greifen wir auf das erste Element in `args` zu, indem wir den Index `[0]` angeben. Beachte, dass dieses Array nullbasiert ist, das heißt, das erste Argument hat immer den Index 0.

Nun, was ist, wenn wir mehrere Argumente übergeben wollen? Kein Problem, wir können einfach eine Schleife verwenden:

```C#
// Annahme: Die übergebenen Argumente sind alle Namen.
for(int i = 0; i < args.Length; i++)
{
    Console.WriteLine("Hallo " + args[i] + "!");
}
```

In diesem Beispiel verwenden wir eine for-Schleife, um jedes Element in `args` zu durchlaufen und die Namen auszugeben. Beachte auch, dass wir `.Length` verwenden, um die Anzahl der Argumente im Array zu erhalten.

## Deep Dive

Jetzt, da du weißt, wie man Befehlszeilenargumente liest, gibt es noch ein paar Dinge, die du wissen solltest. Zum einen kannst du in C# auch Optionen an deine Argumente anhängen, zum Beispiel `-h` für Hilfe oder `-v` für Verbose-Modus. Diese Optionen werden normalerweise mit einem vorangestellten Bindestrich angegeben und du kannst sie durch die Verwendung von `args[i].StartsWith("-")` in deinem Code erkennen.

Außerdem gibt es noch die Möglichkeit, Argumente mit Werten zu versehen. Ein gutes Beispiel dafür sind Konfigurationsdateien. Hier kannst du über die Option `-c` den Pfad zur Konfigurationsdatei angeben und diesen dann in deiner Anwendung auslesen und nutzen.

Eine wichtige Sache, die du beachten solltest, ist, dass Befehlszeilenargumente immer als Strings übergeben werden. Daher musst du sie, je nach Bedarf, in andere Datentypen umwandeln. Zum Beispiel, wenn du eine Anzahl übergeben bekommen möchtest, musst du das entsprechende Argument in einen Integer umwandeln, bevor du es verwenden kannst.

## Siehe auch

[Microsoft Dokumentation für Command Line Arguments in C#](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/main-and-command-args/command-line-arguments)

[Artikel über das Verarbeiten von Befehlszeilenargumenten in C#](https://www.tutorialspoint.com/command-line-arguments-in-c-sharp)