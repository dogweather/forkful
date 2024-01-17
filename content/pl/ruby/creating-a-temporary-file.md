---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Ruby: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Tworzenie tymczasowych plików w programowaniu odnosi się do tworzenia tymczasowego pliku w systemie operacyjnym w celu przechowywania tymczasowych danych. Programiści często używają tymczasowych plików w celu przechowywania danych, które są potrzebne tylko przez krótki czas lub w celu uniknięcia powielania istniejących plików.

## Jak?
```ruby
require 'tempfile'
temp_file = Tempfile.new("temporary_file")
puts temp_file.path
puts temp_file.write("This is a temporary file.")
puts temp_file.read
temp_file.close
```
Output:
```
/tmp/temporary_file20210612-15440-1ofz1g1
23
This is a temporary file.
```

## Pogłębione Zagadnienia
1. Historyczny kontekst: Tworzenie tymczasowych plików jest powszechną praktyką w programowaniu, ale ma swoje korzenie w wcześniejszych systemach operacyjnych, gdzie pamięć operacyjna była ograniczona i programiści musieli ciężko pracować, aby oszczędzać miejsce na dysku.
2. Alternatywy: Istnieją inne metody przechowywania tymczasowych danych, takie jak korzystanie z pamięci RAM lub tworzenie zmiennych tymczasowych w kodzie programu. Jednak używanie tymczasowych plików jest często najbardziej uniwersalne i wygodne.
3. Szczegóły implementacji: W Ruby, klasa Tempfile dostarcza łatwą do używania funkcjonalność do tworzenia tymczasowych plików. Podczas gdy domyślnie plik jest usuwany po zamknięciu, można także wyłączyć automatyczne usuwanie i ręcznie usunąć plik za pomocą metody #unlink.

## Zobacz także
- [Dokumentacja Ruby dla klasy Tempfile](https://docstore.mik.ua/orelly/webprog/pcook/ch13_16.htm)
- [Tutorial dla początkujących o tworzeniu tymczasowych plików w Ruby](https://www.rubyguides.com/2015/07/reading-and-writing-files-in-ruby/)
- [Blog post o używaniu tymczasowych plików w Ruby on Rails](https://www.davidverhasselt.com/create-tmp-directories-and-temporary-files-in-ruby/)