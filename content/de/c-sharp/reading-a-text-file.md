---
title:    "C#: Ein Textdokument lesen"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie jemals versucht haben, eine große Menge an Daten zu verarbeiten, sind Sie wahrscheinlich auf das Lesen von Textdateien gestoßen. Das Lesen einer Textdatei kann Ihnen helfen, wichtige Informationen zu extrahieren und sie in ein übersichtliches Format zu bringen. In diesem Blog-Beitrag erfahren Sie, wie Sie Textdateien in C# lesen können.

## Wie geht das?

Um eine Textdatei in C# zu lesen, müssen Sie zunächst die "System.IO" -Namespacen einbinden. Dann können Sie die Klasse "StreamReader" verwenden, um eine Textdatei zu öffnen und zu lesen. Anschließend können Sie die gelesenen Daten in der Konsole ausgeben.

```C#
using System.IO;
...
StreamReader reader = new StreamReader("textdatei.txt");
string text = reader.ReadToEnd();
Console.WriteLine(text);
```

Wenn Sie eine Textdatei mit Zeilen von Daten haben, können Sie auch die Methode "ReadLine()" verwenden, um jede Zeile einzeln zu lesen und zu verarbeiten. Zum Beispiel:

```C#
using System.IO;
...
StreamReader reader = new StreamReader("textdatei.txt");
string line = "";
while((line = reader.ReadLine()) != null)
{
    // Verarbeiten Sie jede Zeile hier
}
```

## Tiefer Einblick

Beim Lesen von Textdateien ist es wichtig zu beachten, dass C# standardmäßig in ANSI-Kodierung liest. Wenn Ihre Textdatei jedoch in einer anderen Kodierung wie UTF-8 gespeichert ist, müssen Sie beim Öffnen des Textdateipfades auch die entsprechende Kodierung angeben. Zum Beispiel:

```C#
StreamReader reader = new StreamReader("textdatei.txt", Encoding.UTF8);
```

Darüber hinaus gibt es auch die Klasse "StreamWriter", die verwendet werden kann, um Textdateien zu erstellen und zu schreiben. Zusammen mit der Klasse "StreamReader" können Sie so eine Textdatei komplett lesen und ändern.

## Siehe auch

- [Microsoft Dokumentation zu FileStream in C#](https://docs.microsoft.com/en-us/dotnet/standard/io/index)
- [C# Tutorial - Lesen und Schreiben von Textdateien](https://www.tutorialsteacher.com/csharp/csharp-file)
- [Ansatz zum Lesen und Schreiben von Textdateien in C#](https://www.c-sharpcorner.com/UploadFile/mahesh/ReadingTextFilesinCSharp11302005075141AM/ReadingTextFilesinCSharp.aspx)