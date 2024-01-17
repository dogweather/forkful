---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Ruby: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?
Czytanie pliku tekstowego w języku Ruby jest procesem odczytywania tekstu ze specjalnego pliku na komputerze. Programiści używają tej funkcji, aby odczytać zawartość plików tekstowych, np. notatników, plików źródłowych lub wyjścia z innych programów.

## Jak to zrobić:
Czytanie pliku tekstowego w języku Ruby jest proste i może odbywać się na różne sposoby. Przedstawimy tutaj dwa przykłady użycia funkcji `File.open`:

W przykładzie poniżej, odczytujemy zawartość pliku "example.txt" i wyświetlamy ją na ekranie:

```Ruby
File.open("example.txt", "r") do |file|
  puts file.read
end
```

Możemy również odczytywać pliki linijka po linijce, przy użyciu pętli `each_line`, na przykład:

```Ruby
File.open("example.txt", "r") do |file|
  file.each_line do |line|
    puts line
  end
end
```

W obu przypadkach, wyjście będzie wyglądać tak samo, czyli wypisze całą zawartość pliku "example.txt" na ekranie.

## Głębsze zanurzenie:
Funkcja `File.open` została wprowadzona w Ruby 1.9 i zastąpiła starsze funkcje `File.new` lub `File.open`. Aby odczytać plik w starszych wersjach języka, należy użyć jednego z tych dwóch przykładów:

```Ruby
File.new("example.txt", "r") do |file|
  puts file.read
end
```

```Ruby
file = File.open("example.txt", "r")
puts file.read
file.close
```

Alternatywnie, istnieją również inne sposoby na odczytywanie plików w języku Ruby, takie jak użycie biblioteki `CSV` do czytania plików CSV lub użycie funkcji `gets` do czytania użytkownika wejściowego.

## Zobacz również:
- Dokumentacja języka Ruby dotycząca funkcji `File`: https://ruby-doc.org/core-3.0.0/File.html
- Biblioteka `CSV` do czytania plików CSV: https://ruby-doc.org/stdlib-3.0.0/libdoc/csv/rdoc/CSV.html
- Funkcja `gets` do czytania wejścia użytkownika: https://ruby-doc.org/core-3.0.0/Kernel.html#method-i-gets