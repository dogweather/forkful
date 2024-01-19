---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie pliku tekstowego to wydobycie i interpretacja danych zapisanych w pliku. Programiści robią to, aby manipulować danymi, przetwarzać informacje i budować interakcje w swoich aplikacjach.

## Jak to zrobić:

```Bash
# Użyj polecenia 'cat'
cat plik.txt

# Używając polecenia 'less' do czytania długich plików
less plik.txt
```
na przykład, jeżeli plik.txt zawiera linie "Hello, World!", output wygląda tak:
```Bash
Hello, World!
```

## Głębsze zagłębienie

Bash, wprowadzone w 1989 roku, jest jednym z najstarszych języków skryptowych. Inne niż Bash alternatywy do czytania plików tekstowych to Python, Ruby czy PHP. Działanie czytania plików w Bashu polega na buforowaniu zawartości pliku i sekwencyjnym odczytywaniu danych.

## Zobacz też:

* Podstawy Bash: https://ryanstutorials.net/bash-scripting-tutorial/bash-input.php
* Czytanie plików w Pythonie: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
* Czytanie plików w Ruby: https://ruby-doc.org/core-2.5.0/IO.html#method-i-read
* Czytanie plików w PHP: https://www.php.net/manual/en/function.fread.php