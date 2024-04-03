---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:17.875876-07:00
description: "Jak to zrobi\u0107: Standardowa biblioteka Ruby oferuje proste metody\
  \ do sprawdzania, czy katalog istnieje. Oto jak to zrobi\u0107 czystym Ruby, bez\
  \ potrzeby\u2026"
lastmod: '2024-03-13T22:44:35.946687-06:00'
model: gpt-4-0125-preview
summary: Standardowa biblioteka Ruby oferuje proste metody do sprawdzania, czy katalog
  istnieje.
title: Sprawdzanie, czy katalog istnieje
weight: 20
---

## Jak to zrobić:
Standardowa biblioteka Ruby oferuje proste metody do sprawdzania, czy katalog istnieje. Oto jak to zrobić czystym Ruby, bez potrzeby korzystania z bibliotek stron trzecich:

```ruby
require 'fileutils'

# Sprawdź, czy katalog istnieje
if Dir.exist?('/ścieżka/do/katalogu')
  puts 'Katalog istnieje.'
else
  puts 'Katalog nie istnieje.'
end
```
Przykładowy wynik:
```
Katalog istnieje.
```
Lub:
```
Katalog nie istnieje.
```

Oprócz użycia `Dir.exist?`, możesz również wykorzystać metodę `File.directory?`, która zwraca `true`, jeśli podana ścieżka jest katalogiem:

```ruby
if File.directory?('/ścieżka/do/katalogu')
  puts 'Katalog istnieje.'
else
  puts 'Katalog nie istnieje.'
end
```
Obie `Dir.exist?` i `File.directory?` są częścią standardowej biblioteki Ruby i nie wymagają żadnych zewnętrznych gemów do użycia, co czyni je wygodnymi i efektywnymi opcjami do sprawdzania katalogów.
