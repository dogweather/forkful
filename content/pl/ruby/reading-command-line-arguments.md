---
title:                "Odczytywanie argumentów wiersza poleceń"
html_title:           "Ruby: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co to jest i po co to robić?

Czy wiesz, że programiści często potrzebują, by ich programy przyjmowały zmienne wartości bezpośrednio z wiersza poleceń? To właśnie jest czytanie argumentów z wiersza poleceń! Jest to przydatny sposób na dostosowywanie programów dla różnych zastosowań lub wykorzystywanie zmiennych ustawień w miejscu uruchamiania aplikacji.

## Jak to zrobić:

Żeby odczytać argumenty z wiersza poleceń, w Ruby musisz użyć specjalnej metody ARGV. Spójrz na poniższy kod:

```Ruby
puts ARGV
```

Zapisz go jako plik przyklad.rb, a następnie uruchom go w terminalu za pomocą polecenia `ruby przyklad.rb` i przekaż w tym samym wierszu poleceń jakieś zmienne, na przykład `ruby przyklad.rb hello world`. Wynikiem powinno być wyświetlenie tablicy argumentów przekazanych do programu: `["hello","world"]`.

## Deep Dive:

Odczytywanie argumentów z wiersza poleceń jest możliwe dzięki wbudowanej w język Ruby metodzie ARGV, która przekazuje przekazane wartości jako tablicę stringów zapisanych pod systemowym aliasem `ARGV`. Dzięki temu, programista może manipulować argumentami za pomocą znanych już metod tablicowych, np. `pop`, `shift` czy `join`.

Alternatywą dla ARGV jest użycie biblioteki o nazwie `optparse`, która pozwala na bardziej zaawansowane odczytywanie argumentów oraz budowę interfejsów wiersza poleceń.

Warto wiedzieć, że można przekazywać argumenty z wiersza poleceń do programu również przez zmienne globalne typu `$0`, `$*` i `$:`.

## See Also:

- [Dokumentacja Ruby o metodzie ARGV](https://ruby-doc.org/core/ARGF.html)
- [Przykładowa biblioteka optparse](https://github.com/ruby/rdoc/blob/master/lib/rdoc/task.rb)