---
date: 2024-01-20 17:56:41.522154-07:00
description: "Jak to zrobi\u0107: Poni\u017Cej znajdziesz prosty przyk\u0142ad, jak\
  \ u\u017Cywa\u0107 argument\xF3w z linii polece\u0144."
lastmod: '2024-03-13T22:44:35.947671-06:00'
model: gpt-4-1106-preview
summary: "Poni\u017Cej znajdziesz prosty przyk\u0142ad, jak u\u017Cywa\u0107 argument\xF3\
  w z linii polece\u0144."
title: "Odczytywanie argument\xF3w linii polece\u0144"
weight: 23
---

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
