---
title:                "Sprawdzanie istnienia katalogu"
html_title:           "Python: Sprawdzanie istnienia katalogu"
simple_title:         "Sprawdzanie istnienia katalogu"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Sprawdzanie czy katalog istnieje to proces, w którym programista sprawdza czy określony katalog istnieje w systemie plików. Jest to powszechnie stosowana praktyka w celu uniknięcia błędów i zapewnienia odpowiedniego działania programu.

## Jak to zrobić:
```Python
import os
if os.path.exists("/path/to/directory"):
    print("Katalog istnieje!")
else:
    print("Katalog nie istnieje.")
```

Przykładowy wynik:
```Python
Katalog istnieje!
```

## W głąb:
Sprawdzanie czy katalog istnieje jest ważną częścią procesu tworzenia oprogramowania od dawna. Wcześniej, kiedy pliki i katalogi były przechowywane na taśmach magnetycznych, nie było możliwe sprawdzanie istnienia katalogu w systemie plików. Jednak wraz z rozwojem technologii, sprawdzanie istnienia katalogu stało się powszechną praktyką.

Alternatywnym sposobem sprawdzania czy katalog istnieje jest użycie metody `isdir()` z modułu `os.path`. Jest to krótsza i bardziej czytelna wersja kodu, ale nie różni się znacząco od użycia `exists()`. Implementacja systemu plików może również mieć wpływ na dostępność katalogów, na przykład przy użyciu systemu plików FAT, nie będzie możliwe sprawdzenie czy katalog istnieje z wykorzystaniem `exists()`.

## Zobacz też:
- [Dokumentacja Python](https://docs.python.org/3/library/os.path.html#os.path.exists)
- [Porównanie między `exists()` a `isdir()`](https://stackoverflow.com/questions/13566093/whats-the-difference-between-os-path-exists-and-os-path-isdir)