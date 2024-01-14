---
title:                "Ruby: Das Lesen von Befehlszeilenargumenten"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Warum

Das Lesen von Befehlszeilenargumenten ist eine äußerst nützliche Fähigkeit, die Ihnen als Ruby Programmierer helfen kann, effizientere und vielseitigere Anwendungen zu schreiben. In diesem Blog-Beitrag werden wir uns mit der Grundlage des Lesens von Befehlszeilenargumenten beschäftigen und Ihnen einige Tipps geben, wie Sie dies in Ihren Projekten implementieren können.

# Wie

Um Befehlszeilenargumente in Ihrem Ruby-Code zu lesen, müssen Sie die `ARGV`-Variable verwenden. Diese beinhaltet alle durch Leerzeichen getrennten Argumente, die beim Ausführen Ihres Ruby-Skripts übergeben wurden. Hier ist ein Beispiel, wie Sie auf diese Variable zugreifen können:

```ruby
puts ARGV[0] # gibt das erste Argument aus
puts ARGV[1] # gibt das zweite Argument aus
```

Angenommen, Sie haben ein Ruby-Skript namens `greetings.rb`, das Sie wie folgt ausführen: `ruby greetings.rb Hallo Welt`. Der obige Code würde dann "Hallo" und "Welt" als Ausgabe ausgeben. Beachten Sie, dass das erste Argument in der `ARGV`-Variable an Index 0 gespeichert ist.

Sie können auch eine Schleife verwenden, um alle Argumente auszugeben:

```ruby
ARGV.each do |arg|
  puts arg
end
```

# Tieferer Einblick

Es gibt einige zusätzliche Dinge, die Sie beim Lesen von Befehlszeilenargumenten beachten sollten. Zum Beispiel können Sie die Anzahl der übergebenen Argumente überprüfen, indem Sie die `ARGV.length`-Methode verwenden. Sie können auch Argumente anhand ihres Datentyps überprüfen und entsprechende Aktionen ausführen.

Einige gängige Muster, die in der Ruby-Community verwendet werden, sind das Verwenden von Option Flags (z.B. `--help` oder `-v`) und die Verwendung von `OptionParser` zum Parsen von Argumenten. Eine tiefere Beschäftigung mit diesen Konzepten ist jedoch über den Rahmen dieses Blog-Beitrags hinaus.

# Siehe auch

- Offizielle Dokumentation zu `ARGV` von Ruby: https://ruby-doc.org/core-2.7.1/ARGV.html
- Ein Tutorial zum Lesen von Befehlszeilenargumenten in Ruby: https://www.rubyguides.com/2018/05/argv-argument-vector/
- Ein Git-Projekt mit Beispielen für die Verwendung von OptionParser: https://github.com/wbailey/command_line_args_ruby