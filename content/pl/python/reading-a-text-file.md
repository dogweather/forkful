---
title:                "Czytanie pliku tekstowego"
html_title:           "C: Czytanie pliku tekstowego"
simple_title:         "Czytanie pliku tekstowego"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Czytanie pliku tekstowego to proces, dzięki któremu twój program może odczytać zawartość pliku i używać go w swojej pracy. Programiści robią to, gdy chcą pracować z danymi zapisanymi jako tekst, np. komunikować się z innymi programami, analizować dane lub testować swoje kody.

## Jak to zrobić:

Oto prosty kod w Pythonie, który pokazuje, jak odczytać plik tekstowy.

```Python
plik = open('plik.txt', 'r')
print(plik.read())
plik.close()
```
Ten kod otwiera plik o nazwie 'plik.txt', odczytuje całą jego zawartość, a następnie zamyka plik.

## Głębsza analiza

Czytanie plików tekstowych jest fundamentalnym aspektem programowania, praktykowanym jeszcze, zanim istniały takie języki jak Python. Alternatywą dla bezpośredniego odczytu pliku jest skorzystanie z modułu pandas, który pozwala na szybsze i wygodniejsze przetwarzanie plików CSV.

Szczegółem implementacyjnym, o którym warto pamiętać, jest obsługa plików za pomocą tzw. kontekstów, które automatycznie zamykają plik, nawet gdy wystąpi błąd. Oto przykładowy kod:

```Python
with open('plik.txt', 'r') as plik:
    print(plik.read())
```
## Zobacz też:

1. Python Documentation on File I/O: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
2. Python File Handling Cheat Sheet: https://www.pythonforbeginners.com/cheatsheet/python-file-handling
3. W3Schools Tutorial on File Handling in Python: https://www.w3schools.com/python/python_file_handling.asp