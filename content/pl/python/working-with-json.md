---
title:                "Python: Praca z json"
simple_title:         "Praca z json"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/working-with-json.md"
---

{{< edit_this_page >}}

## Dlaczego

JSON, czyli JavaScript Object Notation, jest powszechnie stosowanym formatem do przesyłania danych w aplikacjach internetowych. Jest on bardzo lekki i czytelny dla człowieka, co czyni go idealnym wyborem do przetwarzania i wymiany informacji. Poznanie pracy z JSON może znacznie zwiększyć Twoje możliwości w programowaniu i otworzyć nowe perspektywy w tworzeniu aplikacji.

## Jak to zrobić

### Tworzenie i zapisywanie pliku JSON

```Python
import json

# Tworzenie słownika z danymi
film = {"tytuł": "Pulp Fiction", "reżyser": "Quentin Tarantino", "rok_produkcji": 1994}

# Zapisywanie danych do pliku JSON
with open("film.json", "w") as f:
    json.dump(film, f)
```

### Odczytywanie danych z pliku JSON

```Python
import json

# Odczytywanie danych z pliku JSON
with open("film.json", "r") as f:
    film = json.load(f)

# Wyświetlenie wyników
print("Tytuł:", film["tytuł"])
print("Reżyser:", film["reżyser"])
print("Rok produkcji:", film["rok_produkcji"])
```

Wynik:

```
Tytuł: Pulp Fiction
Reżyser: Quentin Tarantino
Rok produkcji: 1994
```

### Konwersja danych na obiekt JSON

```Python
import json

# Przykładowy słownik z danymi
produkty = {"jabłka": 3, "banany": 2, "gruszki": 5, "pomarańcze": 1}

# Konwersja na obiekt JSON
produkty_json = json.dumps(produkty)

# Wyświetlenie wyniku
print(produkty_json)
```

Wynik:

```
'{"jabłka": 3, "banany": 2, "gruszki": 5, "pomarańcze": 1}'
```

## Deep Dive

Praca z JSON w Pythonie jest bardzo prosta i intuicyjna. Moduł `json` dostarcza kilka przydatnych funkcji, które ułatwiają przetwarzanie i konwersję danych. Istnieje również możliwość importu i eksportu danych z API, dzięki czemu możesz automatycznie pobierać i przetwarzać informacje z innych źródeł.

Podczas pracy z obiektami JSON ważne jest, aby pamiętać o weryfikacji poprawności danych. Możesz to zrobić za pomocą funkcji `json.validate()`, która sprawdza, czy dane spełniają określone wymagania.

## Zobacz także

- [Dokumentacja modułu JSON w Pythonie](https://docs.python.org/3/library/json.html)
- [Praca z JSON w Django](https://docs.djangoproject.com/en/3.2/topics/serialization/)
- [Wyjaśnienie formatu JSON](https://www.json.org/json-en.html)