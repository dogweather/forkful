---
title:    "Python: Sprawdzanie czy istnieje katalog."
keywords: ["Python"]
---

{{< edit_this_page >}}

# Dlaczego

Jeśli jesteś programistą Pythona, zapewne wielokrotnie korzystałeś z różnych modułów i funkcji wbudowanych w ten język. Jedną z nich jest funkcja `os.path.exists()`, która służy do sprawdzania czy dany katalog istnieje. W tym artykule dowiesz się, dlaczego powinieneś wiedzieć, jak sprawdzić istnienie katalogu w swoim kodzie.

# Jak to zrobić

Zacznijmy od tego, co otrzymujemy, używając funkcji `os.path.exists()`. Zwraca ona wartość logiczną `True` lub `False` w zależności od tego, czy dany katalog faktycznie istnieje. Sprawdźmy to przykładem:

```Python
import os

path = 'C:/Users/Username/Documents'

if os.path.exists(path):
    print("Katalog istnieje.")
else:
    print("Katalog nie istnieje.")
```

W powyższym przykładzie za pomocą funkcji `os.path.exists()` sprawdzamy, czy istnieje katalog "Documents" na danej ścieżce. Jeśli tak, wyświetlamy odpowiednie komunikaty. Pamiętaj, że warto podać poprawną ścieżkę do katalogu, który chcesz sprawdzić.

# Głębsza analiza

Jeśli chcesz dowiedzieć się więcej o tym, jak działa funkcja `os.path.exists()`, warto spojrzeć na jej definicję. Zgodnie z dokumentacją Pythona, funkcja ta sprawdza, czy dany podany string jest istniejącą ścieżką systemu plików. Oznacza to, że możemy jej użyć nie tylko do sprawdzania katalogów, ale także plików. Możemy też wykorzystać ją w pętlach `for` lub `while` do sprawdzania wielu katalogów na raz.

Również istnieje wiele innych funkcji, które mogą być przydatne w kontekście sprawdzania istnienia katalogów. Przykładowo, funkcja `os.path.isdir()` sprawdza, czy dana ścieżka jest katalogiem, a `os.path.isfile()` służy do sprawdzania, czy ścieżka wskazuje na plik.

# Zobacz również

- [Dokumentacja Pythona](https://docs.python.org/3/library/os.path.html)
- [Przykłady użycia funkcji os.path w Pythonie](https://www.tutorialspoint.com/python/os_path.htm)
- [Inne przydatne funkcje w module os.path](https://www.geeksforgeeks.org/python-os-path-exists-method/)

Mam nadzieję, że ten artykuł przybliżył Ci temat sprawdzania istnienia katalogów w Pythonie. Pamiętaj, że funkcje `os.path` mogą być bardzo przydatne w wielu różnych projektach, więc warto je poznać i wykorzystywać w swoim kodzie.