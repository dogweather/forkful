---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Czytanie pliku tekstowego to proces, w którym program zbiera dane zapisane w pliku. Programiści robią to w celu manipulacji danymi, analizy czy tworzenia różnych funkcji w swoim kodzie.

## Jak to zrobić:
```Ruby
# Otwórz plik i przeczytaj jego zawartość
File.open("testfile.txt", "r") do |file|
  puts file.read
end
```
Po uruchomieniu tego kodu, dostaniesz wyjście tożsame z zawartością twojego pliku `testfile.txt`.

## Dogłębne rozważania
Historia czytania plików tekstowych w Rubym pokrywa się z historią samego języka. W miarę ewolucji Ruby, możliwości odczytu plików tekstowych również się rozwinęły.

Co do alternatyw, jest wiele innych języków programowania, które możesz wykorzystać do odczytu plików tekstowych. Python, Java, czy JavaScript - każdy z nich posiada swoje metody do pracy z plikami tekstowymi.

W kontekście implementacji, Ruby używa klasy `File` do reprezentowania plików na dysku. Metoda `open` otwiera plik, a `read` czyta go od początku do końca, zwracając zawartość jako jeden długi ciąg znaków.

## Zobacz także
1. [Dokumentacja Ruby - klasa `File`](https://ruby-doc.org/core/File.html)
2. [Jak odczytywać pliki tekstowe w Pythonie](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
3. [Odczytywanie i zapisywanie plików w Java](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
4. [JavaScript - obsługa plików](https://developer.mozilla.org/pl/docs/Web/API/File/Using_files_from_web_applications)