---
title:                "Python: Odczytywanie pliku tekstowego"
simple_title:         "Odczytywanie pliku tekstowego"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zdarzyło Ci się, że musiałeś przeczytać plik tekstowy w języku Python? Jeśli tak, wiesz już prawdopodobnie, że jest to bardzo powszechny scenariusz w świecie programowania. Czytanie plików tekstowych jest ważną umiejętnością i niezbędnym elementem wielu projektów. Zapoznanie się z tą tematyką pozwoli Ci na pracę z różnymi typami danych i dostęp do informacji, które są przechowywane w plikach.

## Jak to zrobić

Najprostszym sposobem na odczytanie pliku tekstowego w języku Python jest użycie funkcji wbudowanej `open`. Wymaga ona dwóch argumentów — nazwy pliku oraz trybu, w jakim plik będzie otwarty (np. tylko do odczytu czy do zapisu). Poniżej przedstawiony jest przykładowy kod, który otwiera plik `tekst.txt` i wypisuje jego zawartość na ekranie:

```Python
file = open("tekst.txt", "r")
print(file.read())
```

W tym przykładzie użyliśmy trybu "r", czyli tylko do odczytu. Możemy także określić inne dostępne tryby, takie jak "w" (zapis) czy "a" (dopisywanie danych na końcu pliku). Po zakończeniu pracy z plikiem, należy pamiętać o jego zamknięciu za pomocą metody `close()`.

## Deep Dive

Podczas czytania plików tekstowych warto pamiętać o kilku istotnych kwestiach. Po pierwsze, zwróć uwagę na dokładną ścieżkę dostępu do pliku, aby program wiedział, gdzie go szukać. Po drugie, należy pamiętać o odpowiednim kodowaniu pliku, ponieważ różne języki mogą wymagać różnych kodowań znaków. W przypadku braku określonego kodowania, przyjętym będzie kodowanie domyślne systemu operacyjnego.

Warto także wiedzieć, że funkcja `open()` zwraca obiekt typu `File`, na którym możemy wywoływać różne metody, takie jak `read()` czy `write()`. Pozwala to na bardziej zaawansowane operacje na plikach, na przykład dzielenie ich na linie czy modyfikowanie zawartości.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o czytaniu plików tekstowych w języku Python, polecamy zapoznać się z poniższymi materiałami:

- Dokumentacja Pythona na temat plików: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Poradnik "Python — odczytywanie pliku tekstowego": https://realpython.com/read-write-files-python/
- Wideo "Python Tutorial: File Objects — Reading and Writing to Files": https://www.youtube.com/watch?v=Uh2ebFW8OYM