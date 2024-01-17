---
title:                "Odczytywanie pliku tekstowego"
html_title:           "Bash: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?
Czytanie pliku tekstowego to proces, w którym program przegląda i odczytuje zawartość istniejącego pliku z tekstem. Programiści często korzystają z tej funkcji, aby uzyskać dostęp do informacji przechowywanych w plikach tekstowych lub przeprowadzić operacje na danych zawartych w takich plikach.

## Jak to zrobić:
Poniżej znajdują się przykładowe kody oraz wyniki wywołania, które pokazują, jak używać polecenia ```cat``` do odczytania zawartości pliku tekstowego, ```grep``` do przefiltrowania wyników i ```wc -l``` do zliczenia ilości linii w pliku.

```Bash
# Przykładowy plik tekstowy "dane.txt":

Imię: Michał
Nazwisko: Nowak
Wiek: 28
Zawód: programista

# Polecenie "cat" odczytuje zawartość pliku tekstowego:
cat dane.txt
```

Wynik:

``` 
Imię: Michał
Nazwisko: Nowak
Wiek: 28
Zawód: programista
```

```Bash
# Polecenie "grep" przefiltrowuje wyniki:
cat dane.txt | grep "Nazwisko"
```
Wynik:
```
Nazwisko: Nowak
```

```Bash
# Polecenie "wc -l" zlicza ilość linii w pliku:
wc -l dane.txt
```
Wynik:
```
4 dane.txt
```

## Deep Dive:
Czytanie plików tekstowych jest jedną z podstawowych funkcji w systemie operacyjnym Unix, którą można wykorzystać do wielu celów, takich jak przetwarzanie danych, analiza logów czy przeszukiwanie danych w plikach. Alternatywnymi sposobami odczytu pliku tekstowego są m.in. wykorzystanie narzędzi jak ```head```, ```tail``` lub ```sed```. Proces czytania pliku odbywa się w dwóch etapach: najpierw system operacyjny odczytuje zawartość pliku do pamięci, a następnie program odczytuje dane z pamięci.

## Zobacz także:
- [Dokumentacja polecenia "cat"](https://www.gnu.org/software/coreutils/manual/html_node/cat-invocation.html)
- [Porównanie polecenia "cat" z innymi narzędziami do odczytywania plików tekstowych](https://www.geeksforgeeks.org/cat-command-in-linux-with-examples/)
- [Inne sposoby odczytu pliku tekstowego w systemie Unix](https://www.lifewire.com/how-to-read-a-text-file-line-by-line-2623446)