---
title:                "Python: Pisanie pliku tekstowego"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego pisanie plików tekstowych jest ważne?

Pisanie plików tekstowych jest niezbędnym elementem w programowaniu Pythona. Pozwala ono na zapisywanie i przechowywanie danych, co jest nieodzowne w wielu aplikacjach. Pisanie plików tekstowych pozwala również na łatwiejsze udostępnianie i przetwarzanie danych pomiędzy różnymi programami.

## Jak to zrobić?

Aby utworzyć nowy plik tekstowy w Pythonie, musimy wykorzystać funkcję `open()`. Pierwszym argumentem funkcji jest nazwa pliku, który chcemy utworzyć lub zmodyfikować. Drugim argumentem jest tryb, w jakim chcemy otworzyć plik. Przykładowy kod wyglądałby następująco:

```Python
with open("nowy_plik.txt", "w") as file:
    file.write("To jest zawartość nowego pliku!")
```

Powyższy kod otwiera plik "nowy_plik.txt" w trybie zapisu ("w") i zapisuje w nim podaną treść. Dzięki użyciu polecenia `with`, plik zostaje automatycznie zamknięty po zakończeniu działania bloku kodu. Istnieją również inne tryby otwierania plików, takie jak "r" (read - tylko do odczytu) czy "a" (append - dopisanie treści do istniejącego pliku).

## Głębsze zagadnienia

Obok podstawowych funkcji, Python oferuje również bardziej zaawansowane metody manipulacji plikami tekstowymi. Na przykład, można użyć polecenia `readlines()` do odczytywania wszystkich wierszy z pliku i zapisania ich w liście. Możemy także określić kodowanie znaków przy otwieraniu pliku, co jest ważne przy pracy z plikami w innych językach niż angielski.

## Zobacz także

- Oficjalna dokumentacja Pythona dotycząca manipulacji plikami: https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files
- Poradnik na temat manipulacji plikami tekstowymi: https://realpython.com/read-write-files-python/
- Przydatne wskazówki dotyczące pracy z plikami w Pythonie: https://www.datacamp.com/community/tutorials/reading-writing-files-python