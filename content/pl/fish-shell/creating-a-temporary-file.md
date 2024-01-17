---
title:                "Tworzenie tymczasowego pliku"
html_title:           "Fish Shell: Tworzenie tymczasowego pliku"
simple_title:         "Tworzenie tymczasowego pliku"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Tworzenie tymczasowych plików jest procesem, w którym programiści tworzą pliki, które są używane tylko przez krótki czas i później usuwane. Jest to często wykorzystywane przez programistów do przechowywania tymczasowych danych lub do przeprowadzania operacji na plikach, aby uniknąć wpływu na istniejące pliki.

## Jak to zrobić?

Tworzenie tymczasowego pliku w Fish Shell nie jest trudne, wystarczy użyć wbudowanego polecenia ```mktemp```. Wywołanie polecenia z przyrostkiem "-d" spowoduje utworzenie tymczasowego katalogu, a bez przyrostka stworzy plik tymczasowy.

Poniżej znajdują się przykłady kodu, które pokazują jak utworzyć tymczasowy plik i jak go wykorzystać:

```
Fish Shell:

# Tworzenie tymczasowego pliku
$ mktemp

# Tworzenie tymczasowego katalogu
$ mktemp -d
```

Wynikiem wykonania powyższego kodu będzie wyświetlenie ścieżki do utworzonego pliku lub katalogu.

## W głębi

Tworzenie tymczasowych plików jest popularną praktyką w świecie programowania. Po raz pierwszy została wprowadzona w systemie operacyjnym UNIX w latach 70., a od tego czasu jest wykorzystywana w wielu językach i narzędziach programistycznych.

Alternatywną metodą tworzenia tymczasowych plików jest użycie polecenia ```tempfile```, jednak jest ono dostępne tylko w niektórych systemach operacyjnych.

W implementacji tworzenia tymczasowego pliku w Fish Shell, wykorzystywane są funkcje systemowe, które zapewniają bezpieczne i niepowtarzalne nazwy dla plików i katalogów tymczasowych.

## Zobacz również

- Dokumentacja Fish Shell dotycząca polecenia ```mktemp```: [link](https://fishshell.com/docs/current/cmds/mktemp.html)
- Dokumentacja systemu operacyjnego UNIX dotycząca tworzenia plików tymczasowych: [link](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Live-Processes)
- Porównanie metod tworzenia tymczasowych plików w różnych językach programowania: [link](https://stackoverflow.com/questions/2336242/how-to-create-a-temporary-file-in-c)