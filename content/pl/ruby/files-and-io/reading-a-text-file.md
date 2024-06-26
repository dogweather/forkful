---
date: 2024-01-20 17:54:57.368306-07:00
description: "How to: (Jak to zrobi\u0107:) Przyk\u0142adowe wyj\u015Bcie."
lastmod: '2024-04-05T21:53:37.379069-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Przyk\u0142adowe wyj\u015Bcie."
title: Odczytywanie pliku tekstowego
weight: 22
---

## How to: (Jak to zrobić:)
```ruby
# Prosty przykład czytania całego pliku
file_content = File.read('example.txt')
puts file_content

# Czytanie pliku linia po linii
File.foreach('example.txt') do |line|
  puts line
end

# Bezpieczne otwieranie pliku z blokiem
File.open('example.txt', 'r') do |file|
  while line = file.gets
    puts line
  end
end
```
Przykładowe wyjście:
```
Witaj, Świecie!
To jest przykładowa linia tekstu.
```

## Deep Dive (Dogłębna analiza)
Czytanie plików tekstowych w Rubym jest proste, ale ma ciekawe tło. Metody takie jak `File.read` są wygodne, ale ładowanie dużych plików może być problemem dla pamięci. Dlatego `File.foreach` i bloki `File.open` są cenne, pozwalają na odczytywanie plików kawałek po kawałku. 

Jest kilka alternatyw do wbudowanych metod Ruby'ego. Biblioteka `CSV` pozwala na obsługę plików CSV, a `FileUtils` zawiera narzędzia do bardziej zaawansowanych operacji na plikach. W Ruby 1.8 i starszych `IO.foreach` był częściej używany, teraz `File.foreach` jest bardziej popularny i wydajny.

## See Also (Zobacz również)
- Dokumentacja Ruby API dla klasy `File`: https://ruby-doc.org/core/File.html
- Tutorial o obsłudze plików w Ruby: https://www.rubyguides.com/2015/05/working-with-files-ruby/
- Ruby-Doc dla `IO` klasy: https://ruby-doc.org/core/IO.html
