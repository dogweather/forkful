---
title:                "Ruby: Lesen von Befehlszeilenargumenten"
simple_title:         "Lesen von Befehlszeilenargumenten"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Warum

Das Lesen von Befehlszeilenargumenten ist eine wichtige Fähigkeit für jeden, der Ruby programmiert und auf der Kommandozeile arbeitet. Durch die Verwendung von Befehlszeilenargumenten kann ein Ruby-Programm mit variablen oder benutzerdefinierten Eingaben ausgeführt werden, ohne dass der Code selbst geändert werden muss. Es ist auch ein effektiver Weg, um die Interaktion mit dem Benutzer zu ermöglichen.

## Wie

Die Verwendung von Befehlszeilenargumenten in Ruby ist sehr einfach. Zunächst muss der "ARGV" Array verwendet werden, um die Argumente aufzunehmen, die beim Aufruf des Programms übergeben werden. Ein Beispielcode dafür sieht so aus:

```Ruby
# Beispielcode für Befehlszeilenargumente
puts "Hallo, #{ARGV[0]}!"
```

Wenn das obige Beispiel als "ruby beispiel.rb Welt" aufgerufen wird, wird es "Hallo, Welt!" ausgeben. Das erste Argument nach dem Programmnamen wird demnach in das Programm übergeben.

## Deep Dive

In Ruby gibt es auch die Möglichkeit, Optionen und Flags über die Befehlszeilenargumente zu übergeben. Diese können mit der Ruby "OptionParser" Klasse einfach gehandhabt werden. Im folgenden Beispielcode werden wir die Option "-u" für den Benutzernamen und die Option "-p" für das Passwort verwenden:

```Ruby
# Beispielcode für Befehlszeilenargumente mit Optionen
require 'optparse'
options = {}
OptionParser.new do |opts|
  opts.banner = "Usage: beispiel.rb [options]"
  opts.on("-u", "--user USER", "Benutzername") do |u|
    options[:user] = u
  end
  opts.on("-p", "--password PASS", "Passwort") do |p|
    options[:password] = p
  end
end.parse!

puts "Benutzername: #{options[:user]}"
puts "Passwort: #{options[:password]}"
```

Wenn das obige Beispiel als "ruby beispiel.rb -u Max -p geheim" aufgerufen wird, wird es "Benutzername: Max" und "Passwort: geheim" ausgeben.

## Siehe auch

- [Ruby Dokumentation zu Befehlszeilenargumenten](https://ruby-doc.org/core-2.7.1/ARGV.html)
- [Ruby Dokumentation zu OptionParser](https://ruby-doc.org/stdlib-2.7.1/libdoc/optparse/rdoc/OptionParser.html)
- [Tutorial zu Befehlszeilenargumenten in Ruby](https://www.rubyguides.com/2019/02/ruby-command-line-arguments/)