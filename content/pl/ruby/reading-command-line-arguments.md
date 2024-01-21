---
title:                "Odczytywanie argumentów linii poleceń"
date:                  2024-01-20T17:56:41.522154-07:00
model:                 gpt-4-1106-preview
simple_title:         "Odczytywanie argumentów linii poleceń"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

W Ruby, czytanie argumentów linii poleceń pozwala twojemu skryptowi przyjmować dane wejściowe z konsoli. Programiści używają tej techniki, by uczynić skrypty bardziej elastycznymi i konfigurowalnymi przez użytkowników bez zmiany samego kodu.

## Jak to zrobić:

Poniżej znajdziesz prosty przykład, jak używać argumentów z linii poleceń:

```Ruby
# hello.rb
if ARGV.length > 0
  name = ARGV.join(' ') # Łączy wszystkie argumenty w jedno zdanie
  puts "Cześć, #{name}!"
else
  puts "Cześć, nieznajomy!"
end
```

Użycie skryptu z linii poleceń:

```
$ ruby hello.rb
Cześć, nieznajomy!

$ ruby hello.rb Jan Kowalski
Cześć, Jan Kowalski!
```

## Deep Dive

Argumenty linii poleceń są tak stare, jak same systemy operacyjne z interfejsem tekstowym. Już pierwsze programy pozwalały na przesyłanie danych podczas uruchamiania programu.

W Ruby, argumenty te są dostępne za pomocą globalnej stałej `ARGV`, która jest tablicą stringów. Argumenty są po prostu ciągiem znaków, więc pamiętaj, żeby przekształcić je w odpowiedni typ danych, jeśli potrzebujesz (np. `to_i` dla liczb całkowitych).

Alternatywy dla `ARGV` obejmują użycie gemów, takich jak `OptionParser` lub `Thor`, które dostarczają bardziej zaawansowane opcje dla parsowania argumentów, takie jak flagi czy opcje z wartościami.

## See Also

Sprawdź te zasoby, aby dowiedzieć się więcej o pracy z argumentami w Ruby:

- Dokumentacja Ruby na temat Argumentów Linii Poleceń: https://www.ruby-lang.org/pl/documentation/quickstart/3/
- Ruby `OptionParser` dokumentacja: https://ruby-doc.org/stdlib-3.0.0/libdoc/optparse/rdoc/OptionParser.html
- Wprowadzenie do gemu `Thor`: http://whatisthor.com/