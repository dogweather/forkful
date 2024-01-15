---
title:                "Eine Textdatei schreiben"
html_title:           "C#: Eine Textdatei schreiben"
simple_title:         "Eine Textdatei schreiben"
programming_language: "C#"
category:             "C#"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man überhaupt in Erwägung ziehen, ein Textdokument mit C# zu schreiben? Nun, manche Programmanwendungen erfordern, dass Text in eine Datei geschrieben wird, um ihn später wieder zu lesen oder zu bearbeiten. Zum Beispiel können Benutzereinstellungen oder Dateilisten auf diese Weise gespeichert werden.

## Wie geht man vor

Um ein Textdokument mit C# zu schreiben, können wir die `StreamWriter`-Klasse verwenden. Hier ist ein Beispielcode, der einen einfachen Text in eine Datei mit dem Namen "beispiel.txt" schreibt:

```C#
using (StreamWriter writer = new StreamWriter("beispiel.txt"))
{
    writer.WriteLine("Dies ist ein Beispieltext.");
}
```

Hier wird der `StreamWriter` in einer `using`-Anweisung verwendet, um sicherzustellen, dass die Ressourcen frei gegeben werden, wenn wir fertig sind. Innerhalb der Anweisung schreiben wir eine Zeile Text mit der `WriteLine`-Methode. Wenn die Datei bereits existiert, wird sie überschrieben. Wenn nicht, wird sie automatisch erstellt.

Wie man sieht, ist es recht einfach, mit C# eine Textdatei zu schreiben. Man kann natürlich auch komplexere Dateien wie HTML, XML oder CSV schreiben, indem man statt einfacher Textzeilen diese Dateiformate formatiert.

## Eintauchen ins Detail

Jetzt wollen wir einen Blick auf einige der features werfen, die wir bei der Verwendung des `StreamWriter` entdecken können.

### Kodierung

Der `StreamWriter` hat eine überladene Konstruktor-Methode, die wir verwenden können, um eine bestimmte Kodierung für unsere Datei anzugeben. Standardmäßig verwendet er UTF-8-Format, aber wir können dies ändern, indem wir einen Kodierungsparameter angeben. Zum Beispiel:

```C#
using (StreamWriter writer = new StreamWriter("beispiel.txt", Encoding.Unicode))
{
    // Text hier schreiben
}
```

### Textkonvertierung

Manchmal wollen wir, dass der `StreamWriter` Zeichen oder Strings in unseren Text konvertiert, bevor er ihn in die Datei schreibt. Dafür gibt es verschiedene Methoden wie `Write`, `WriteLine` und `WriteAsync`, die entweder einen `Char`, eine `String`, eine `Bool`, eine `Double` usw. als Parameter akzeptieren. Wenn wir beispielsweise eine Zeichenkette in eine Zeile schreiben wollen, können wir dies tun:

```C#
string name = "Max Mustermann";
writer.WriteLine($"Mein Name ist {name}.");
```

### Verwendung mit anderen .NET-Klassen

Der `StreamWriter` kann auch mit anderen .NET-Klassen kombiniert werden, um Text auf verschiedene Arten zu lesen und zu schreiben. Zum Beispiel können wir einen `StreamReader` verwenden, um Text aus einer Datei zu lesen und diesen dann in einen `StreamWriter` zu schreiben, um ihn in eine andere Datei zu kopieren.

## Siehe auch

- [MSDN-Dokumentation zum StreamWriter](https://docs.microsoft.com/de-de/dotnet/api/system.io.streamwriter?view=net-5.0)
- [Tutorial: Lesen und Schreiben von Dateien (C#-Programmierhandbuch)](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/file-system/how-to-read-from-a-text-file)
- [Dateien lesen und schreiben in C# (Codeburst)](https://codeburst.io/working-with-files-in-c-b017ef5c3f18)