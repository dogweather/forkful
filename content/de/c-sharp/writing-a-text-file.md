---
title:    "C#: Ein Textfile schreiben"
keywords: ["C#"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum man als Programmierer Textdateien schreiben möchte. Einer davon könnte sein, dass man Daten in einem einfach zugänglichen Format speichern möchte, das von anderen Programmen leicht gelesen werden kann. Man könnte auch eine Protokolldatei erstellen, um nachvollziehen zu können, was im Programm passiert ist. Egal aus welchem Grund, das Schreiben einer Textdatei ist eine grundlegende Fähigkeit, die jeder C#-Programmierer beherrschen sollte.

## Wie
Um eine Textdatei mit C# zu schreiben, gibt es einige Schritte, die du beachten musst. Zuerst musst du eine Datei erstellen und sie mit dem entsprechenden Pfad versehen. Dort wird die Textdatei gespeichert. Dann öffnest du die Datei und schreibst den gewünschten Text in sie hinein. Vergiss nicht die Datei am Ende zu schließen, um sicherzustellen, dass alle Daten gespeichert werden.

```C#
//Datei erstellen und Pfad angeben
string path = @"C:\Users\Benutzer\Desktop\txtDatei.txt";
//Datei öffnen
FileStream fs = File.Open(path, FileMode.OpenOrCreate);
//In die Datei schreiben
byte[] text = new UTF8Encoding(true).GetBytes("Das ist ein Beispieltext");
fs.Write(text, 0, text.Length);
//Datei schließen
fs.Close();
```

Die obigen Zeilen erstellen eine Textdatei mit dem Namen "txtDatei" auf dem Desktop des Benutzers und schreiben den Text "Das ist ein Beispieltext" in die Datei.

## Tiefer Einblick
Wenn du tiefer in das Schreiben von Textdateien mit C# eintauchen möchtest, gibt es noch weitere Funktionen und Optionen, die du nutzen kannst. Zum Beispiel kannst du überprüfen, ob eine Datei bereits existiert, bevor du versuchst sie zu öffnen. Dafür kannst du die Methode `File.Exists()` verwenden. Außerdem kannst du, anstatt einen `FileStream` zu verwenden, die Methode `File.WriteAllText()` nutzen, um direkt den Text in deine Datei zu schreiben.

See Also
- [File Class (C#-Referenz)](https://docs.microsoft.com/de-de/dotnet/api/system.io.file?view=net-5.0)
- [How to: Write to a Text File (C# Programming Guide)](https://docs.microsoft.com/de-de/dotnet/csharp/programming-guide/file-system/how-to-write-to-a-text-file)