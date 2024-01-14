---
title:    "Python: Sprawdzanie istnienia katalogu"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Dlaczego

Często w trakcie programowania, potrzebujemy sprawdzić, czy dany folder istnieje w naszym systemie plików. Może to być potrzebne do wykonania konkretnej manipulacji na plikach lub po prostu w celu upewnienia się, że nasz program nie będzie próbował działać na nieistniejącym folderze. W tym artykule dowiesz się, jak przeprowadzić takie sprawdzenie.

## Jak to zrobić

Sprawdzenie, czy dany folder istnieje w naszym systemie plików, jest prostym procesem w języku Python. Możemy użyć wbudowanego modułu `os` oraz funkcji `path.exists()`, aby sprawdzić istnienie danego folderu. Przykładowy kod wyglądałby następująco:

```Python
import os

if os.path.exists("nazwa_folderu"):
    print("Folder istnieje.")
else:
    print("Folder nie istnieje.")
```

Jeśli folder o nazwie "nazwa_folderu" istnieje w obecnej lokalizacji, to na ekranie zostanie wyświetlony komunikat "Folder istnieje.", w przeciwnym wypadku zostanie wyświetlony komunikat "Folder nie istnieje.".

Możemy także użyć funkcji `path.isdir()` lub `path.isfile()` do sprawdzenia, czy dany folder lub plik istnieją. Przykładowy kod wykorzystujący te funkcje wyglądałby następująco:

```Python
import os

if os.path.isdir("nazwa_folderu"):
    print("To jest folder.")
elif os.path.isfile("nazwa_folderu"):
    print("To jest plik.")
else:
    print("Nie ma takiego folderu ani pliku.")
```

W powyższym przykładzie najpierw sprawdzamy, czy dany element jest folderem, jeśli nie jest, to sprawdzamy, czy jest plikiem, a jeśli nie jest ani jednym, ani drugim, wyświetlamy odpowiedni komunikat.

## Deep Dive

Podczas wykonywania powyższych przykładów, możemy zauważyć, że funkcja `path.exists()` sprawdza zarówno istnienie folderu, jak i pliku. Innymi słowy, jeśli w podanej lokalizacji znajduje się zarówno folder o nazwie "nazwa_folderu", jak i plik o tej samej nazwie, to funkcja zwróci wartość True.

Warto także zwrócić uwagę, że w powyższych przykładach używamy względnej ścieżki do folderu. Oznacza to, że folder "nazwa_folderu" znajduje się w obecnej lokalizacji, w której znajduje się nasz skrypt. Jeśli chcemy przeszukać system plików od głównego katalogu, musimy podać pełną ścieżkę, na przykład `os.path.exists("/home/użytkownik/nazwa_folderu")`.

Możemy także używać operatora `not` do odwrócenia warunku, na przykład `if not os.path.exists("nazwa_folderu")`, aby sprawdzić, czy dany folder nie istnieje.

## Zobacz też

- Oficjalna dokumentacja języka Python: [https://docs.python.org/pl/3/library/os.path.html](https://docs.python.org/pl/3/library/os.path.html)
- Przewodnik po modułach wbudowanych w języku Python: [https://docs.python.org/pl/3/tutorial/modules.html](https://docs.python.org/pl/3/tutorial/modules.html)
- Poradnik na stronie Real Python o manipulacji plikami i folderami: [https://realpython.com/working-with-files-in-python/](https://realpython.com/working-with-files-in-python/)