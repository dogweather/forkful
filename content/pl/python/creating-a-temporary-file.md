---
title:    "Python: Tworzenie tymczasowego pliku"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Dlaczego

Stworzenie pliku tymczasowego jest częstym elementem programowania w Pythonie, a ta umiejętność może przydać się w wielu sytuacjach. Na przykład, jeśli tworzysz program, który wymaga zapisywania danych w pliku, ale nie chcesz trwale zmieniać danych w istniejącym pliku, plik tymczasowy może być idealnym rozwiązaniem. Może to również być przydatne przy testowaniu różnych funkcji w programie, ponieważ plik tymczasowy można łatwo utworzyć i usunąć, co ułatwia debugowanie.

## Jak to zrobić

Stworzenie pliku tymczasowego w Pythonie jest proste i można to zrobić z użyciem modułu `tempfile`. Najpierw musimy zaimportować ten moduł, a następnie wywołać funkcję `TemporaryFile()`.

```Python
import tempfile

# Utworzenie pliku tymczasowego
with tempfile.TemporaryFile() as tmp:
    # Operacje na pliku tymczasowym
    tmp.write(b'Hello, world!')
    tmp.seek(0)
    print(tmp.read())

# Plik tymczasowy zostanie automatycznie usunięty po wyjściu z bloku "with"
```

W powyższym przykładzie, po utworzeniu pliku tymczasowego, używamy funkcji `write()` do zapisania napisu "Hello, world!" w pliku. Następnie używamy funkcji `seek()` do ustawienia wskaźnika na początek pliku, a następnie funkcja `read()` odczytuje zawartość pliku i wyświetla ją w konsoli.

## Deep Dive

Funkcja `TemporaryFile()` może również przyjmować pewne argumenty, które umożliwiają dostosowanie pliku tymczasowego. Na przykład, możemy określić tryb dostępu do pliku (`'w+'` oznacza tryb pisania i odczytu) oraz `delete=False`, aby plik tymczasowy nie został automatycznie usunięty po opuszczeniu bloku "with".

```Python
import tempfile

# Utworzenie pliku tymczasowego z określonymi argumentami
with tempfile.TemporaryFile(mode='w+', delete=False) as tmp:
    # Operacje na pliku tymczasowym
    tmp.write('Hello, world!')
    tmp.seek(0)
    print(tmp.read())

# Plik tymczasowy nie zostanie usunięty po wyjściu z bloku "with"
# Musimy go ręcznie usunąć za pomocą funkcji "unlink()"
tmp.unlink()
```

Funkcja `TemporaryFile()` zwraca obiekt typu `NamedTemporaryFile`, który może być używany w podobny sposób do pliku tymczasowego.

## Zobacz także

- Dokumentacja modułu `tempfile`: https://docs.python.org/3/library/tempfile.html
- Poradnik na temat tworzenia plików tymczasowych w Pythonie: https://realpython.com/python-tempfile/
- Przydatne szablony dla plików tymczasowych: https://github.com/python/cpython/blob/master/Lib/tempfile.py