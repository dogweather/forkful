---
title:                "Einsatz eines Debuggers"
date:                  2024-01-26T04:09:35.915177-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einsatz eines Debuggers"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/using-a-debugger.md"
---

{{< edit_this_page >}}

## Was & Warum?

Die Verwendung eines Debuggers in Ruby verleiht Programmierern eine Superkraft, um ihren Code anzuhalten, Variablen zu inspizieren und ihren Code Zeile für Zeile durchzugehen. Leute machen das, um Bugs zu beseitigen, den Codefluss zu verstehen und genau zu sehen, was ihre geschriebenen Zauberformeln (Code) tun, wenn die Magie passiert – oder auch nicht.

## Wie geht das:

Ruby kommt mit einem eingebauten Debugger namens `byebug`. Zuerst füge `byebug` in deiner Gemfile hinzu und führe `bundle install` aus. Dann setz einfach `byebug` genau dort ein, wo du möchtest, dass dein Programm eine Pause einlegt.

```Ruby
require 'byebug'

def calculate_magic(number)
  byebug
  magische_zahl = number * 7
  return magische_zahl
end

puts calculate_magic(6)
```

Wenn du dieses Skript ausführst, wird die Ausführung bei `byebug` angehalten und du wirst in eine interaktive Sitzung geworfen, in der du Befehle wie eingeben kannst:

```
step
next
continue
var local
```

Die Beispiel-Ausgabe würde dir ein Prompt wie folgt geben:

```
[2, 11] in beispiel.rb
    2: 
    3: def calculate_magic(number)
    4:   byebug
=>  5:   magische_zahl = number * 7
    6:   return magische_zahl
    7: end
    8: 
    9: puts calculate_magic(6)
(byebug) 
```

## Vertiefung:

Weit zurück, vor `byebug`, nutzten Rubyisten `debugger` und `pry`. Letzteres, `pry`, ist mehr als ein Debugger; es ist ein leistungsfähiger REPL, der auch zum Debuggen mit dem Breakpoint `binding.pry` verwendet werden kann.

Alternativen zu Rubys `byebug` sind `pry-byebug`, das `pry` mit `byebug` Funktionalität kombiniert, und `ruby-debug`, welches ein älteres, nicht aktiv gepflegtes Gem ist.

Wenn du `byebug` aufrufst, unterbricht der Debugger die Ausführung deines Codes und gibt dir einen Einblick in die Laufzeit. Du kannst Variablen sehen und ändern, zu verschiedenen Punkten im Code springen und sogar etwas Ruby-Code Zeile für Zeile ausführen. Es ist so, als hätte man Zeitreisefähigkeiten für deinen Ruby-Code.

## Siehe auch:

- Byebug GitHub-Repository: [https://github.com/deivid-rodriguez/byebug](https://github.com/deivid-rodriguez/byebug)
- Pry Dokumentation: [https://github.com/pry/pry](https://github.com/pry/pry)
- Ein Leitfaden zum Debuggen von Rails-Apps: [https://guides.rubyonrails.org/debugging_rails_applications.html](https://guides.rubyonrails.org/debugging_rails_applications.html)
