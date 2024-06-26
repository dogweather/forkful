---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:03.765315-07:00
description: "Jak to zrobi\u0107: Ruby sprawia, \u017Ce operacje na plikach s\u0105\
  \ proste. Aby zapisa\u0107 do pliku, mo\u017Cna u\u017Cy\u0107 wbudowanej klasy\
  \ `File` w Ruby. Poni\u017Cszy przyk\u0142ad\u2026"
lastmod: '2024-03-13T22:44:35.950790-06:00'
model: gpt-4-0125-preview
summary: "Ruby sprawia, \u017Ce operacje na plikach s\u0105 proste."
title: Pisanie pliku tekstowego
weight: 24
---

## Jak to zrobić:
Ruby sprawia, że operacje na plikach są proste. Aby zapisać do pliku, można użyć wbudowanej klasy `File` w Ruby. Poniższy przykład demonstruje, jak otworzyć plik do zapisu (tryb `"w"`) oraz do dodawania (tryb `"a"`), następnie zapisać do niego ciąg znaków i upewnić się, że plik zostanie zamknięty po zakończeniu:

```ruby
# Zapisywanie nowej zawartości do pliku, nadpisywanie istniejącej zawartości
File.open("example.txt", "w") do |file|
  file.puts "Witaj, Ruby!"
end

# Dodawanie zawartości na końcu pliku
File.open("example.txt", "a") do |file|
  file.puts "Dodaję kolejną linię."
end
```
Po uruchomieniu obu fragmentów kodu, zawartość `example.txt` będzie wyglądać tak:
```
Witaj, Ruby!
Dodaję kolejną linię.
```

### Korzystanie z biblioteki zewnętrznej: FileUtils
Do bardziej złożonych operacji na plikach przydaje się standardowa biblioteka Ruby `FileUtils`, chociaż do podstawowego zapisu w plikach wystarczają standardowe metody klasy `File`. Jednakże, jeżeli chcesz kopiować, przenosić, usuwać lub wykonywać inne operacje na systemie plików w połączeniu z zapisem plików, warto zapoznać się z `FileUtils`.

Przykład użycia `FileUtils` do utworzenia katalogu, a następnie zapisu do pliku w tym katalogu:
```ruby
require 'fileutils'

FileUtils.mkdir_p 'logs'
File.open("logs/dziennik.log", "w") do |file|
  file.puts "Wpis do dziennika: #{Time.now}"
end
```

To demonstruje tworzenie nowego katalogu `logs`, jeśli jeszcze nie istnieje, i zapisywanie do nowego pliku `dziennik.log` w nim, pokazując manipulację zarówno katalogami, jak i plikami bez bezpośredniego zapisywania za pomocą FileUtils, ale wykorzystując jego zdolności do obsługi katalogów.
