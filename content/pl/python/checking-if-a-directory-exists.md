---
title:                "Python: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego?

W dzisiejszych czasach, w których pracujemy z różnymi systemami operacyjnymi, często musimy mieć do czynienia z plikami i folderami. Sprawdzanie, czy dany katalog istnieje, może być kluczowym elementem w naszym kodzie, aby upewnić się, że operacja, którą chcemy przeprowadzić, może być wykonana.

## Jak to zrobić?

Aby sprawdzić, czy dany katalog istnieje w naszym systemie plików, możemy użyć biblioteki os w języku Python. Służy ona do interakcji z systemem plików i dostarcza różne funkcje do zarządzania plikami i folderami. Aby sprawdzić, czy dany katalog istnieje, możemy wykorzystać funkcję `path.isdir()` w następujący sposób:

```Python
import os

if os.path.isdir("moj_katalog"):
    print("Katalog istnieje")
else:
    print("Katalog nie istnieje")
```

Jeśli katalog o nazwie "moj_katalog" istnieje w naszym bieżącym katalogu, to na ekranie zostanie wyświetlony komunikat "Katalog istnieje". W przeciwnym razie, zostanie wyświetlony komunikat "Katalog nie istnieje".

## Głębsze zanurzenie

Podczas korzystania z biblioteki os do sprawdzania istnienia katalogu, warto zauważyć, że funkcja `path.isdir()` przyjmuje jako argument ścieżkę do katalogu, a nie samą nazwę katalogu. Możemy więc określić ścieżkę względną do naszego katalogu lub użyć funkcji `path.abspath()` do uzyskania pełnej ścieżki.

Ponadto, możemy również wykorzystać funkcję `path.exists()` do sprawdzenia istnienia pliku lub katalogu, bez względu na jego typ. Możemy także wykorzystać funkcję `path.isfile()` do sprawdzenia, czy dany plik istnieje.

## Zobacz także

- Dokumentacja biblioteki os w języku Python: https://docs.python.org/3/library/os.html
- Inne sposoby sprawdzania istnienia katalogów w Pythonie: https://www.geeksforgeeks.org/python-os-path-isdir-method/