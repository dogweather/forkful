---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie plików tymczasowych, to praktyka znana niemal każdemu programiście. Pliki tymczasowe służą jako bufor dla danych, które są zbyt duże, aby przechowywać je w pamięci albo które muszą być zachowane przez krótki czas między operacjami.

## Jak to zrobić:

W Ruby tworzyć pliki tymczasowe jest bardzo proste dzięki wbudowanej bibliotece 'tempfile'. Oto proste przykładowe użycie:

```Ruby
require 'tempfile'

tempfile = Tempfile.new('mojplik')
puts tempfile.path      # Wypisuje ścieżkę do pliku na dysku

tempfile.puts("Witaj, świecie!")
tempfile.rewind         # Przewija plik do początku

puts tempfile.read      # Wypisuje: "Witaj, świecie!"
tempfile.close
```

## Pogłębione spojrzenie

Tworzenie plików tymczasowych to technika wykorzystywana od początków informatyki. Ułatwia ona prace z dużymi ilościami danych oraz zapewnia bezpieczeństwo, dzięki izolacji procesów.

Jako alternatywę dla 'tempfile', można bezpośrednio obsługiwać system plików, ale to jest bardziej skomplikowane i podatne na błędy. Tempfile automatycznie dba o czyszczenie i usuwanie plików po ich użyciu.

Szczegół implementacyjny: Tempfile tworzy pliki w katalogu określonym przez zmienną środowiskową `TMPDIR` lub `/tmp` jeśli `TMPDIR` nie jest ustawione.

## Zobacz też

Dokumentacja Tempfile: <https://ruby-doc.org/stdlib-2.5.1/libdoc/tempfile/rdoc/Tempfile.html>

Krótki tutorial na StackOverflow: <https://stackoverflow.com/questions/6959214/how-do-i-make-a-temporary-file-in-ruby>

Dokumentacja Ruby File and I/O: <https://ruby-doc.org/core-2.2.0/File.html>