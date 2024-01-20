---
title:                "Tworzenie tymczasowego pliku"
html_title:           "C#: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Tworzenie tymczasowych plików jest procesem, który polega na generowaniu tymczasowej przestrzeni do przechowywania danych. Programiści tworzą je w celu przechowywania informacji, które mogą być potrzebne tylko podczas jednej sesji lub procedury.

## Jak to Zrobić:
Najprostszym sposobem na stworzenie tymczasowego pliku w Bash jest użycie polecenia `mktemp`. Przykład:
```Bash
temp_file=$(mktemp)
echo "To jest przykładowa zawartość." > "$temp_file"
cat "$temp_file"
```
Po uruchomieniu kodu powyżej, zostanie wygenerowany tymczasowy plik, wpis do niego tekstu "To jest przykładowa zawartość." a następnie wyświetlonego z zawartością pliku.

## Głębsze Zanurzenie:
Tworzenie tymczasowych plików ma swoje korzenie w starszych systemach operacyjnych, które wykorzystywały takie pliki do przechowywania tymczasowych danych podczas wykonywania długotrwałych operacji. Istnieją alternatywy dla `mktemp`, takie jak utworzenie pliku o unikalnej nazwie za pomocą kombinacji daty i czasu. Szczegół implementacji polecenia `mktemp` polega na tworzeniu pliku w katalogu /tmp systemu Linux.

## Zobacz Również:
1. [Tworzenie plików tymczasowych w Pythonie](http://python-examples.com/python-tempfile/)
2. [Tworzenie plików tymczasowych w C++](http://cpp-examples.com/cpp-tempfile/)
3. [Dokumentacja Bash](https://www.gnu.org/software/bash/manual/bash.html)
4. [Dokumentacja mktemp](https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html)