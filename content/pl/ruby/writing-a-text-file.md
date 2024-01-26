---
title:                "Zapisywanie pliku tekstowego"
html_title:           "Arduino: Zapisywanie pliku tekstowego"
simple_title:         "Zapisywanie pliku tekstowego"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Zapisywanie do pliku tekstowego to proces przechowywania danych w standardowym formacie tekstowym. Programiści robią to dla trwałego zapisu informacji, wymiany danych między aplikacjami lub do logowania.

## Jak to zrobić:

```Ruby
# Tworzenie i zapisywanie do pliku
File.open('przyklad.txt', 'w') do |plik|
  plik.puts "Witaj, Świecie!"
end

# Dodawanie do istniejącego pliku
File.open('przyklad.txt', 'a') do |plik|
  plik.puts "Do zobaczenia."
end

# Wczytywanie i wyświetlanie zawartości pliku
puts File.read('przyklad.txt')
```

Przykładowe wyjście po uruchomieniu kodu:
```
Witaj, Świecie!
Do zobaczenia.
```

## Deep Dive

Pisanie do pliku tekstowego jest jednym z podstawowych mechanizmów I/O dostępnych w Rubym. Istnieją także inne metody, takie jak `IO.write` czy `File.write`, które pozwalają na zapis bez bloku. Istotne, by pamiętać o trybach dostępu - `'w'` nadpisze plik, `'a'` doda zawartość na końcu. 

## Zobacz również

- Dokumentacja Ruby IO: https://ruby-doc.org/core-3.1.0/IO.html
- Dokumentacja Ruby File: https://ruby-doc.org/core-3.1.0/File.html
- Tutorial do obsługi plików w Ruby: https://www.rubyguides.com/2015/05/working-with-files-ruby/
