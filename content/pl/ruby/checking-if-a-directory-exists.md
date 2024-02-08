---
title:                "Sprawdzanie, czy katalog istnieje"
aliases:
- pl/ruby/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:17.875876-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sprawdzanie, czy katalog istnieje"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i Dlaczego?
Sprawdzanie, czy katalog istnieje w Ruby, pozwala programistom na weryfikację obecności katalogu przed wykonaniem operacji takich jak odczytywanie plików czy tworzenie nowych katalogów. Jest to kluczowe dla uniknięcia błędów w obsłudze plików i zapewnienia niezawodności manipulacji systemem plików.

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
