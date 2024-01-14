---
title:                "Python: Sprawdzanie, czy istnieje katalog"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

"Czy istnieje katalog w Pythonie? Przewodnik dla początkujących"

## Dlaczego

Sprawdzanie, czy katalog istnieje, jest ważną częścią programowania w Pythonie. To pozwala nam na upewnienie się, czy ścieżka, którą chcemy użyć w naszym kodzie, jest prawidłowa i czy nie powoduje błędów podczas działania programu.

## Jak to zrobić

Istnieje kilka różnych sposobów, aby sprawdzić, czy katalog istnieje w Pythonie. Jednym z najprostszych jest użycie funkcji `os.path.exists()`. Spójrzmy na poniższy kod:

```Python
import os

# ścieżka do katalogu, który chcemy sprawdzić
path = "C:/Users/Example/Desktop"

if os.path.exists(path):
  print("Katalog istnieje!")
else:
  print("Katalog nie istnieje!")
```

W tym przykładzie użyliśmy funkcji `os.path.exists()`, aby sprawdzić, czy podana ścieżka istnieje. Jeśli tak, zostanie wyświetlony komunikat "Katalog istnieje!", a jeśli nie, zostanie wyświetlony komunikat "Katalog nie istnieje!".

Możemy również użyć funkcji `os.path.isdir()` aby upewnić się, że podana ścieżka jest katalogiem, nie tylko plikiem. Poniżej przedstawiony jest przykładowy kod:

```Python
import os

# ścieżka do katalogu, który chcemy sprawdzić
path = "C:/Users/Example/Documents"

if os.path.isdir(path):
  print("To jest katalog!")
else:
  print("To nie jest katalog!")
```

W tym przypadku sprawdziliśmy, czy podana ścieżka jest katalogiem, a następnie wyświetliliśmy odpowiedni komunikat.

## Pogłębione zagadnienia

W Pythonie istnieje wiele innych funkcji i metod, które możemy użyć do sprawdzania istnienia katalogów, takich jak `os.path.isfile()`, `os.path.islink()`, czy `os.path.getsize()`. Warto zapoznać się z dokumentacją Pythona, aby poznać wszystkie dostępne opcje.

## Zobacz również

- Dokumentacja Pythona o modułach `os` i `os.path`: https://docs.python.org/3/library/os.html
- Przewodnik programowania Python dla początkujących: https://www.learnpython.org/pl/