---
title:    "Ruby: Lesen von Befehlszeilenargumenten"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

 Es gibt viele Gründe, warum das Lesen von Befehlszeilenargumenten wichtig und hilfreich ist. Zum Beispiel ermöglicht es die Gestaltung interaktiver Programme, die vom Benutzer auf der Kommandozeile gesteuert werden können. Es kann auch beim Debuggen und Testen von Code hilfreich sein.

## Wie man Befehlszeilenargumente liest

Um Befehlszeilenargumente in Ruby zu lesen, gibt es eine eingebaute Methode namens `ARGV`. Diese Liste enthält alle Argumente, die beim Ausführen des Skripts übergeben wurden.

```Ruby
# Beispielcode zum Lesen von Befehlszeilenargumenten
# Aufruf: ruby read_args.rb arg1 arg2

puts "Argument 1: #{ARGV[0]}" 
puts "Argument 2: #{ARGV[1]}"

# Output:
# Argument 1: arg1
# Argument 2: arg2
```

Wenn keine Argumente übergeben werden, ist die `ARGV`-Liste einfach leer.

## Tiefentauchen

Es gibt noch einige weitere nützliche Funktionen und Möglichkeiten im Zusammenhang mit dem Lesen von Befehlszeilenargumenten. Zum Beispiel können mit `ARGV.count` die Anzahl der übergebenen Argumente und mit `ARGV.join(' ')` alle Argumente zu einem einzigen String zusammengefügt werden.

Eine weitere interessante Möglichkeit ist die Verwendung von sogenannten Flags, die bestimmte Optionen beim Ausführen des Skripts auswählen. Diese können mit der `OptionParser`-Klasse implementiert werden, um das Lesen von Befehlszeilenargumenten noch flexibler und inhaltlich besser strukturiert zu gestalten.

## Siehe auch

- [Offizielle Ruby-Dokumentation zu ARGV](https://ruby-doc.org/core-2.7.1/ARGV.html)
- [Tutorial: Command Line Arguments in Ruby](https://www.rubyguides.com/2018/08/ruby-command-line-arguments/)
- [Using Command Line Arguments with Ruby](https://www.sitepoint.com/using-command-line-arguments-with-ruby/)