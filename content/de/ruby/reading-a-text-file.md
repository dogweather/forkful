---
title:                "Ruby: Eine Textdatei lesen."
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Lesen von Textdateien ist eine wichtige Fähigkeit, die jeder Ruby-Programmierer beherrschen sollte. Es ermöglicht das Einlesen und Verarbeiten von großen Mengen an Daten, die in Form von Textdateien vorliegen.

## Wie geht das?

Das Lesen einer Textdatei in Ruby ist ein relativ einfacher Prozess. Zunächst muss die Datei mit der entsprechenden Erweiterung (.txt oder .csv) im richtigen Verzeichnis platziert werden. Dann kann der folgende Code verwendet werden, um die Datei zu öffnen:

```Ruby
file = File.open("textdatei.txt")
```

Um den Inhalt der Datei zu lesen, können wir die `each`-Methode verwenden, die eine Schleife durchläuft und jede Zeile der Datei als `line`-Variable speichert. 

```Ruby
file.each do |line|
puts line
end
```

Dieser Code liest jede Zeile der textdatei.txt-Datei und gibt sie in der Konsole aus. Wenn wir beispielsweise den Inhalt der Datei "textdatei.txt" haben:

```
Hallo! 
Wie geht es dir? 
Schön dich kennenzulernen.
```

Die Ausgabe des obigen Codes wäre:

```
Hallo!
Wie geht es dir?
Schön dich kennenzulernen.
```

## Tiefer eintauchen

Es gibt verschiedene Methoden, um eine Textdatei in Ruby zu lesen, je nach den Anforderungen des Codes. Eine andere Möglichkeit ist die Verwendung der `gets`-Methode, die es uns ermöglicht, Benutzereingaben direkt aus der Konsole zu lesen:

```Ruby
puts "Bitte gib deinen Namen ein:"
name = gets.chomp
puts "Hallo #{name}, schön dich kennen zu lernen!"
```

Wenn wir nun den Namen "Anna" als Eingabe eingeben, wäre die Ausgabe:

```
Bitte gib deinen Namen ein:
Anna
Hallo Anna, schön dich kennen zu lernen!
```

## Siehe auch

- <a href="https://www.geeksforgeeks.org/ruby-file-io-methods/" target="_blank">Ruby File IO Methods</a>
- <a href="https://www.rubyguides.com/2015/05/working-with-files-ruby/" target="_blank">Working with Files in Ruby</a>
- <a href="https://www.tutorialspoint.com/ruby/ruby_input_output.htm" target="_blank">Ruby Input/Output</a>