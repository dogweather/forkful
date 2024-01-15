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

## Dlaczego

Tworzenie pliku tymczasowego jest nieodzownym elementem w wielu projektach programistycznych. Dzięki temu możemy tymczasowo przechować dane, a następnie usunąć je po zakończeniu potrzebnych operacji.

## Jak to zrobić

Tworzenie plików tymczasowych w języku Python jest bardzo proste. Możemy wykorzystać do tego moduł `tempfile`, który oferuje wiele funkcji ułatwiających pracę z plikami tymczasowymi.

```Python
import tempfile

# Tworzenie pliku tymczasowego
temp_file = tempfile.TemporaryFile()

# Zapisywanie danych do pliku
temp_file.write(b"Hello World!")

# Odczyt danych z pliku
temp_file.seek(0)
print(temp_file.read())

# Usuwanie pliku
temp_file.close()
```

W powyższym przykładzie `TemporaryFile()` tworzy plik tymczasowy, który jest usuwany po wywołaniu metody `close()`. Istnieją także inne funkcje, takie jak `NamedTemporaryFile()` czy `SpooledTemporaryFile()`, które oferują dodatkowe możliwości pracy z plikami tymczasowymi.

## Głębszy zanurzenie

Pliki tymczasowe są tworzone w systemie operacyjnym na zasadzie "od razu, na miejscu". Oznacza to, że plik zostanie utworzony w dedykowanym folderze systemowym (np. /tmp w systemach Unix) i będzie dostępny pod wygenerowaną nazwą. W przypadku wykorzystania funkcji `NamedTemporaryFile()` możemy również sami nadać nazwę plikowi.

Istnieje także możliwość wskazania folderu, w którym zostanie utworzony plik tymczasowy, poprzez argument `dir` w funkcji `TemporaryFile()` lub `NamedTemporaryFile()`. W przypadku chęci zapisania większej ilości danych do pliku tymczasowego, warto rozważyć wykorzystanie funkcji `SpooledTemporaryFile()`, która najpierw zapisuje dane w pamięci podręcznej, a następnie zapisuje je do pliku tymczasowego po wywołaniu metody `flush()`, co pozwala uniknąć operacji zapisu na dysku.

## Zobacz też

- [Dokumentacja oficjalna modułu tempfile w Python](https://docs.python.org/3/library/tempfile.html)
- [Przykładowe wykorzystanie plików tymczasowych w Python](https://www.geeksforgeeks.org/temporary-files-python/)