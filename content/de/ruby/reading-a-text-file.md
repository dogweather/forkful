---
title:    "Ruby: Ein Textdokument lesen"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Textdateien ist eine wichtige Fähigkeit, die jeder Ruby-Programmierer beherrschen sollte. Mit dieser Funktion können Sie Daten aus externen Quellen in Ihr Programm einbinden und verarbeiten. Das kann hilfreich sein, wenn Sie beispielsweise Benutzereingaben aus einer Datei lesen oder externe Daten in Ihrem Programm verwenden möchten. In diesem Blogpost erfahren Sie, wie Sie Textdateien in Ruby lesen und welche Vorteile dies bietet.

## Wie man es macht

Das Lesen einer Textdatei in Ruby ist relativ einfach und erfordert nur wenige Zeilen Code. Zunächst müssen Sie die Datei mit dem Befehl `File.open()` öffnen und eine Variable zuweisen, um auf die Datei zugreifen zu können. Zum Beispiel:

```Ruby
file = File.open("textfile.txt")
```

Anschließend können Sie die Daten aus der Datei lesen, indem Sie die `read` Methode verwenden. Diese Funktion gibt den gesamten Inhalt der Datei zurück. Eine mögliche Implementierung könnte folgendermaßen aussehen:

```Ruby
file = File.open("textfile.txt")
puts file.read
```

Dies würde den gesamten Inhalt der Datei auf der Konsole ausgeben.

## Tiefergehende Informationen

Beim Lesen einer Textdatei gibt es einige Dinge zu beachten. Zum Beispiel müssen Sie sicherstellen, dass die Datei, auf die Sie zugreifen möchten, an der richtigen Stelle im Dateisystem liegt. Sie können auch angeben, ob Sie die Datei nur zum Lesen oder zum Schreiben öffnen möchten, indem Sie die entsprechenden Parameter an `File.open()` übergeben. Darüber hinaus können Sie mit der `readlines` Methode die Datei zeilenweise lesen und mit der `foreach` Methode jede Zeile der Datei iterativ verarbeiten.

In Ruby gibt es auch einige eingebaute Methoden und Funktionen, mit denen Sie den Inhalt der Datei verarbeiten können, indem Sie z.B. Zeichenketten suchen, ersetzen oder trennen.

Nun wissen Sie, wie einfach es ist, eine Textdatei in Ruby zu lesen und wie Sie die Daten weiterverarbeiten können.

## Siehe auch

Hier sind einige hilfreiche Links, um weitere Informationen über das Lesen von Textdateien in Ruby zu erhalten:

- [Offizielle Ruby Dokumentation](https://ruby-doc.org/core-2.7.0/File.html)
- [Tutorial zum Lesen und Schreiben von Dateien in Ruby](https://www.rubyguides.com/2015/05/working-with-files-ruby/)
- [GNU Ruby Text Editor (Regexp)](http://rubyregexp.sourceforge.net/)

Viel Spaß beim Programmieren mit Ruby!