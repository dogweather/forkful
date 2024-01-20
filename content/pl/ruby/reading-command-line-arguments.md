---
title:                "Czytanie argumentów linii poleceń"
html_title:           "Bash: Czytanie argumentów linii poleceń"
simple_title:         "Czytanie argumentów linii poleceń"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Czytanie Argumentów Linii Poleceń w Ruby

## Co i dlaczego?

Czytanie argumentów linii poleceń to proces gromadzenia informacji wprowadzonych przez użytkowników podczas wywoływania skryptów. Dzięki temu, programiści mogą dynamicznie manipulować funkcjami i zachowaniem skryptów.

## Jak to zrobić:

W Ruby, argumenty linii poleceń są dostępne za pomocą specjalnej tablicy ARGV. Kiedy uruchamiasz skrypt, wszystko po nazwie skryptu traktowane jest jako argumenty i zapisywane do tablicy ARGV.

Poniżej znajduje się przykład.

```ruby
# plik: args.rb
puts "You gave #{ARGV.size} arguments"
ARGV.each_with_index do |arg, index|
  puts "Argument #{index + 1}: #{arg}"
end
```

Przykładowe wyjście, kiedy uruchomisz `ruby args.rb arg1 arg2 arg3`:

```
You gave 3 arguments
Argument 1: arg1
Argument 2: arg2
Argument 3: arg3
```

## Deep Dive

Czytanie argumentów linii poleceń ma wiele zastosowań, od zmiany zachowania skryptu po przetwarzanie danych. W historii programowania, jest to funkcja dostępna w prawie każdym języku, pozwalająca na interakcję użytkownika z kodem.

W Ruby, alternatywą do użycia ARGV jest użycie biblioteki `OptionParser`, która umożliwia bardziej zaawansowane manipulacje z argumentami.

Co do implementacji, Ruby traktuje argumenty linii poleceń jako ciągi znaków. ARGV jest tablicą tych ciągów, a indeksy tablicy odpowiadają kolejności, w której argumenty zostały wprowadzone.

## Zobacz także:

- Dokumentacja Ruby ARGV: https://ruby-doc.org/core-2.1.4/ARGV.html
- Ruby OptionParser: https://ruby-doc.com/stdlib/libdoc/optparse/rdoc/OptionParser.html
- Przewodnik po argumentach linii poleceń: https://www.learnenough.com/command-line-tutorial/command-line-arguments