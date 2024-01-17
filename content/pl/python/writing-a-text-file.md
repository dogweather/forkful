---
title:                "Pisanie pliku tekstowego"
html_title:           "Python: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Zapisywanie pliku tekstowego w języku Python jest jednym z podstawowych zadań, z którymi spotkasz się jako programista. Polega to na zapisaniu tekstu lub innych danych w pliku, który można potem odczytać lub edytować. Jest to niezbędne, gdy chcesz zachować dane na trwałe lub je przetwarzać w dalszym ciągu.

## Jak to zrobić:
Kodując przy użyciu języka Python, istnieją różne sposoby zapisania pliku tekstowego. Możesz użyć funkcji wbudowanej `open()` lub modułu `io`. Przykładowy kod wykorzystujący funkcję `open()` wygląda tak:
```Python
file = open("plik.txt", "w")
file.write("To jest przykładowy tekst.")
file.close()
```
W wyniku tego kodu zostanie stworzony plik o nazwie "plik.txt", a w nim pojawi się napis "To jest przykładowy tekst.". Ważne jest, aby na końcu wywołać funkcję `close()`, aby zapisać zmiany i zwolnić zasoby.

## Głębsze zagadnienia:
Początki zapisu plików tekstowych sięgają czasów DOS-a, kiedy to programiści tworzyli skrypty w języku Batch. Wtedy również używano funkcji `open()`, jednak niektórych opcji, takich jak kodowanie znaków, nie było. Alternatywą dla funkcji `open()` jest użycie modułu `io`, który oferuje bardziej rozbudowane możliwości. Implementacja zapisu pliku tekstowego opiera się na otwarciu pliku w odpowiednim trybie, zapisaniu danych i zamknięciu pliku. Ważne jest również, aby pamiętać o obsłudze wyjątków, w przypadku gdy operacja zapisu nie powiedzie się.

## Zobacz też:
- Dokumentacja Pythona na temat zapisywania danych: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Wprowadzenie do manipulacji plikami w języku Python: https://realpython.com/read-write-files-python/