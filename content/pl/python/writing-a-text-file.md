---
title:                "Python: Pisanie pliku tekstowego"
simple_title:         "Pisanie pliku tekstowego"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Pisanie plików tekstowych jest bardzo ważną częścią nauki programowania w języku Python. Jest to niezbędna umiejętność dla każdego programisty, niezależnie od jego poziomu doświadczenia. Tworzenie i modyfikowanie plików tekstowych jest kluczowym elementem w wielu projektach i może pomóc w automatyzacji zadań oraz przechowywaniu danych.

## Jak to zrobić

Aby stworzyć plik tekstowy w języku Python, możemy użyć funkcji `open()` wraz z parametrem `"w"` (write), która umożliwia zapisywanie danych do pliku. Następnie, możemy użyć metody `write()` aby wprowadzić zawartość do pliku. Na przykład:

```Python
file = open("moj_plik.txt", "w")
file.write("To jest test")
file.close()
```

Ten kod stworzy plik o nazwie "moj_plik.txt" i wprowadzi w nim tekst "To jest test". Po wykonaniu tego kodu, plik ten będzie dostępny dla nas w tym samym folderze, w którym znajduje się plik z kodem.

Jeśli chcemy odczytać zawartość pliku, możemy użyć metody `read()` w następujący sposób:

```Python
file = open("moj_plik.txt", "r")
print(file.read())
file.close()
```

Wykonanie tego kodu spowoduje wyświetlenie zawartości pliku w konsoli.

## Wnikliwe spojrzenie

Pisanie plików tekstowych jest tylko jednym ze sposobów, w jaki Python może przetwarzać dane. Możemy również użyć biblioteki `csv` aby pracować z plikami formatu CSV lub biblioteki `json` do obsługi plików w formacie JSON. Język Python oferuje również wiele innych funkcji związanych z pracą na plikach, takich jak modyfikowanie, aktualizacja i usuwanie danych.

Jednym z najważniejszych aspektów pisania plików tekstowych jest pamiętanie o zamknięciu pliku po zakończeniu jego pracy. Możemy to zrobić za pomocą metody `close()`, która zwalnia zasoby używane przez ten plik.

Zalecamy również korzystanie z konstrukcji `with...as`, która automatycznie zamknie plik po zakończeniu bloku kodu. Na przykład:

```Python
with open("moj_plik.txt", "w") as file:
    file.write("To jest test")
```

Ta konstrukcja jest korzystna szczególnie w przypadku, gdy pracujemy z wieloma plikami jednocześnie.

## Zobacz także

- Dokumentacja Pythona o operacjach na plikach: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Tutoriale na YouTube: https://www.youtube.com/watch?v=Uh2ebFW8OYM
- Przykładowy kod pisania i odczytywania plików w Pythonie: https://realpython.com/read-write-files-python/