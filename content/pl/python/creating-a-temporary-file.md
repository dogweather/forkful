---
title:                "Tworzenie pliku tymczasowego"
html_title:           "Python: Tworzenie pliku tymczasowego"
simple_title:         "Tworzenie pliku tymczasowego"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?

Tworzenie tymczasowych plików jest częstą czynnością w programowaniu, ponieważ pozwala na tymczasowe zapisanie danych, które są potrzebne podczas pracy programu. Często są one używane do przechowywania informacji, które będą później usunięte lub zmodyfikowane, bez wpływu na resztę programu.

## Jak to zrobić:

```Python
import tempfile

# Tworzenie tymczasowego pliku i zapisanie danych
with tempfile.TemporaryFile() as tmp:
  tmp.write(b"Hello, world!")
  # Jeśli nie podamy trybu, domyślnie zostanie użyty tryb binarny
  tmp.seek(0) # Przejście na początek pliku
  print(tmp.read())

# Tworzenie tymczasowego folderu i zapisanie pliku wewnątrz
with tempfile.TemporaryDirectory() as tmp_dir:
  tmp_path = tempfile.mkstemp(dir=tmp_dir, prefix="temp_", suffix=".txt")[1]
  # mkstemp zwraca tuplę, z której interesuje nas drugi element - ścieżka do pliku
  with open(tmp_path, 'w') as tmp_file:
    tmp_file.write("Hello, world!")
    print(tmp_file.read())
```

## Wnikliwe spojrzenie:

Tworzenie tymczasowych plików jest popularne od dawna, ponieważ umożliwia programistom sprawnie i bezpiecznie manipulować danymi w trakcie działania programu. Alternatywami mogą być na przykład przechowywanie danych w pamięci lub w bazie danych, jednak nie zawsze są one odpowiednie lub wygodne. Implementacja jest wysoce zależna od środowiska, w którym pracujemy - na przykład stworzenie pliku tymczasowego na systemie operacyjnym Unix będzie wyglądać inaczej niż na Windowsie.

## Zobacz też:

Dokumentacja modułu `tempfile` w bibliotece standardowej Pythona: https://docs.python.org/3/library/tempfile.html
Wprowadzenie do manipulacji plikami w Pythonie: http://realpython.com/read-write-files-python/
Porównanie różnych sposobów tworzenia tymczasowych plików: http://zen-engine.appspot.com/readwrite-files.htm