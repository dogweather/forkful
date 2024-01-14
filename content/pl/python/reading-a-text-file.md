---
title:                "Python: Odczytywanie pliku tekstowego"
programming_language: "Python"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek musiałeś przeczytać plik tekstowy w swoim kodzie Python? Jeśli tak, to dobrze wiesz, że jest to ważna umiejętność, która może ułatwić pracę z danymi w Twoim programie. W tym blogu dowiesz się, dlaczego warto nauczyć się czytać pliki tekstowe w Pythonie.

## Jak to zrobić

Aby przeczytać plik tekstowy w Pythonie, możesz wykorzystać funkcję wbudowaną `open()`. Spójrzmy na prosty przykład:

```Python
with open("plik.txt", "r") as f:
    for line in f:
        print(line)
```

Powyższy kod otwiera plik o nazwie "plik.txt" w trybie tylko do odczytu (`"r"`) i przypisuje go do zmiennej `f`. Następnie pętla `for` iteruje po wszystkich liniach w pliku i wypisuje je na konsoli. 

Możesz również określić inny tryb otwarcia pliku, na przykład `"w"` dla zapisu lub `"a"` dla dopisywania. Zawsze pamiętaj, aby zamknąć plik po użyciu poprzez wywołanie metody `close()` lub korzystając z instrukcji `with`, co zapewni automatyczne zamknięcie pliku.

## Deep Dive

Podczas czytania plików w Pythonie warto zwrócić uwagę na kilka rzeczy. Po pierwsze, tryb otwarcia powinien zostać właściwie określony, aby móc później wykonać żądane operacje na pliku. Ponadto, pamiętaj, że metoda `readline()` pozwala na odczyt pojedynczej linii, natomiast `readlines()` zwraca listę z wszystkimi liniami. 

Innym przydatnym narzędziem jest moduł `csv`, który umożliwia łatwiejszą pracę z plikami CSV. Możesz je odczytywać i zapisywać przy użyciu specjalnych funkcji, takich jak `reader()` i `writer()`.

## Zobacz również

Jeśli chcesz poznać więcej o pracy z plikami tekstowymi w Pythonie, polecamy zapoznać się z oficjalną dokumentacją języka Python oraz z następującymi linkami:

- [Inne przydatne funkcje do czytania plików w Pythonie](https://www.w3schools.com/python/python_file_handling.asp)
- [Podstawy modułu csv w Pythonie](https://realpython.com/python-csv/)
- [Przykład wykorzystania instrukcji with](https://www.geeksforgeeks.org/with-statement-in-python/)