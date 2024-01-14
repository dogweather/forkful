---
title:                "Python: Praca z plikami csv"
simple_title:         "Praca z plikami csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Dlaczego

Praca z plikami CSV jest niezbędna w wielu dziedzinach, takich jak analiza danych, tworzenie raportów lub praca z bazą danych. Python jest powszechnie używany do manipulowania plikami CSV, ponieważ jest łatwy w użyciu i dostępny dla wszystkich. W tym wpisie przeprowadzimy Cię przez podstawy pracy z plikami CSV w Pythonie, abyś mógł zacząć wykorzystywać je w swoich projektach!

## Jak to zrobić

Python ma wbudowaną bibliotekę `csv`, która umożliwia nam łatwe parsowanie plików CSV. Najpierw musimy zaimportować tę bibliotekę:

```python
import csv
```

Następnie, aby otworzyć plik CSV, musimy użyć funkcji `open` i przekazać jej ścieżkę do naszego pliku oraz tryb "read" (`"r"`):

```python
with open("plik.csv", "r") as plik:
    # kod
```

Teraz, gdy nasz plik jest otwarty, możemy użyć funkcji `reader` z biblioteki `csv`, aby przeczytać plik i zapisać go w zmiennej:

```python
with open("plik.csv", "r") as plik:
    czytnik = csv.reader(plik)
```

Teraz nasza zmienna `czytnik` zawiera całą zawartość pliku CSV, w formie tabeli. Możemy na przykład wypisać całą zawartość tabeli:

```python
with open("plik.csv", "r") as plik:
    czytnik = csv.reader(plik)

    for wiersz in czytnik:
        print(wiersz)
```

Kod ten wypisze wszystkie wiersze w naszym pliku CSV. Aby wyświetlić tylko wybrane kolumny lub wartości, możemy użyć indeksowania, na przykład `wiersz[0]` wyświetli pierwszą kolumnę.

Jednak nie zawsze otrzymujemy idealnie sformatowany plik CSV, niekiedy musimy usunąć nagłówki lub puste wiersze, lub zmienić separator. W takich przypadkach możemy użyć funkcji `DictReader`, która pozwala nam odwoływać się do wartości wierszy po nazwie kolumy, a nie po indeksie:

```python
with open("plik.csv", "r") as plik:
    czytnik = csv.DictReader(plik)

    for wiersz in czytnik:
        print(wiersz["kolumna"])
```

Pamiętaj, że jeśli zastosowaliśmy zmiany w naszym pliku CSV, należy go zapisać przed zamknięciem:

```python
with open("plik.csv", "w") as plik:
    writer = csv.writer(plik)
    # kod

    writer.writerows() # zapisuje zmiany
```

## Głębsza analiza

Pliki CSV są popularnym narzędziem do przechowywania i udostępniania danych, ale są też narażone na błędy podczas przekazywania lub modyfikowania. Dlatego ważne jest, aby pamiętać o podstawowych zasadach pracy z plikami CSV:

- Sprawdź format pliku - czy używany jest odpowiedni separator i czy pole tekstowe jest ograniczone cudzysłowami.
- Usuń puste wiersze i kolumny, aby uniknąć wprowadzania błędów do analizy danych.
- Uważaj na nazwy kolumn, aby nie zawierały polskich znaków lub znaków specjalnych.

Pamiętaj również, że Python oferuje wiele innych bibliotek do pracy z plikami CSV, takich jak `pandas` czy `numpy`, dzięki którym możemy wygodniej analizować i przetwarzać nasze dane.

## Zobacz także

- [Dokumentacja biblioteki csv w Pythonie](