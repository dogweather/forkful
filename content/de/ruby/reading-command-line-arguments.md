---
title:    "Ruby: Lesen von Befehlszeilenargumenten"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Warum
Es gibt viele Gründe, warum es nützlich sein kann, Befehlszeilenargumente in Ruby zu lesen. Zum Beispiel können sie verwendet werden, um bestimmte Einstellungen oder Konfigurationen für ein Programm festzulegen. Sie ermöglichen es auch dem Benutzer, bestimmte Aktionen auszulösen oder Parameter zu übergeben, um bestimmte Ergebnisse zu erhalten. Darüber hinaus kann das Lesen von Befehlszeilenargumenten auch hilfreich sein, um Programme flexibler und leichter anpassbar zu gestalten.

## Wie geht das?
In Ruby können Befehlszeilenargumente über die `ARGV`-Konstante gelesen werden. Diese enthält ein Array mit allen übergebenen Argumenten. Um auf spezifische Argumente zuzugreifen, kann man die Indexposition des Arrays verwenden. Hier ist ein Beispiel, wie man auf das erste Argument zugreifen kann:

```Ruby
puts "Das erste Argument ist #{ARGV[0]}"
```

Wenn wir nun unser Programm mit einem Argument beim Aufrufen ausführen, z.B. `ruby programm.rb Hallo`, wird der Text "Das erste Argument ist Hallo" ausgegeben.

## Tiefergehende Analyse
Das Lesen von Befehlszeilenargumenten kann in Ruby noch weiter verfeinert werden, indem man Optionen und Argumente mithilfe von OptionParser analysiert. Diese Klasse bietet eine einfache Möglichkeit, Befehlszeilenargumente zu lesen und zu verarbeiten. Hier ist ein Beispiel, wie man OptionParser verwenden kann:

```Ruby
require 'optparse'

options = {}

OptionParser.new do |opts|
  opts.banner = "Verwendung: programm.rb [-h] [-v]"

  opts.on('-h', '--hilfe', 'Zeigt die Hilfe an') do
    puts opts
    exit
  end

  opts.on('-v', '--version', 'Zeigt die Versionsnummer an') do
    puts "Programmname 1.0.0"
    exit
  end
end.parse!

puts "Weitere Argumente: #{ARGV.join(', ')}"
```

Hier können wir sehen, dass wir mithilfe von OptionParser auch Optionen wie z.B. `-h` und `-v` definieren können, um bestimmte Aktionen auszuführen. Der `opts.banner` gibt außerdem eine kurze Hilfe aus, wenn das Programm ohne Argumente oder mit `-h` aufgerufen wird.

## Siehe auch
- [Ruby-befehlszeilenargumente - Dokumentation von Ruby](https://docs.ruby-lang.org/de/2.5.0/OptionParser.html)
- [OptionParser Tutorial -  Der offizielle Ruby-on-Rails-Leitfaden](https://guides.rubyonrails.org/command_line.html#creating-a-script)