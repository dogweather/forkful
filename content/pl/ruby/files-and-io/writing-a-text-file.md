---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:03.765315-07:00
description: "Zapisywanie do pliku tekstowego w Ruby jest podstawow\u0105 operacj\u0105\
  , kt\xF3ra pozwala na trwa\u0142e przechowywanie wyj\u015Bcia i danych, umo\u017C\
  liwiaj\u0105c p\xF3\u017Aniejszy dost\u0119p\u2026"
lastmod: 2024-02-19 22:04:55.134351
model: gpt-4-0125-preview
summary: "Zapisywanie do pliku tekstowego w Ruby jest podstawow\u0105 operacj\u0105\
  , kt\xF3ra pozwala na trwa\u0142e przechowywanie wyj\u015Bcia i danych, umo\u017C\
  liwiaj\u0105c p\xF3\u017Aniejszy dost\u0119p\u2026"
title: Pisanie pliku tekstowego
---

{{< edit_this_page >}}

## Co i dlaczego?
Zapisywanie do pliku tekstowego w Ruby jest podstawową operacją, która pozwala na trwałe przechowywanie wyjścia i danych, umożliwiając późniejszy dostęp do danych lub ich modyfikację. Programiści często wykonują to zadanie z różnych powodów, takich jak logowanie, zapisywanie konfiguracji czy eksport danych w formacie łatwym do odczytu dla człowieka.

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
