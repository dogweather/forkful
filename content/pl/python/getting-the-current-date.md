---
title:                "Pobieranie bieżącej daty"
html_title:           "Python: Pobieranie bieżącej daty"
simple_title:         "Pobieranie bieżącej daty"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Co i Dlaczego?
Pobieranie bieżącej daty w Pythonie jest procesem, w którym programista uzyskuje informację o bieżącym dniu, miesiącu, roku i czasie. Obejmuje to również pobieranie informacji o strefie czasowej, w której znajduje się urządzenie. Programiści używają tego zazwyczaj w celu śledzenia zmiany czasu lub do synchronizacji wydarzeń.

## Jak to zrobić:
W Pythonie istnieje wiele sposobów na uzyskanie bieżącej daty. Zaprezentujemy kilka z nich poniżej, a także wyjaśnienia, jak czytać i interpretować wynik.

```python
# Metoda 1: Importowanie modułu datetime
import datetime

# Używanie funkcji datetime.now() do pobrania bieżącej daty i czasu
current_date = datetime.now()

# Wyświetlanie wyniku jako obiekt datetime
print(current_date)
```
Wynik:
```
2021-11-22 13:30:45.086682
```

```python
# Metoda 2: Używanie funkcji date.today()
from datetime import date

# Pobieranie bieżącej daty
current_date = date.today()

# Wyświetlanie tylko daty bez czasu
print(current_date)
```
Wynik:
```
2021-11-22
```

```python
# Metoda 3: Używanie funkcji time.time()
import time

# Pobieranie liczby sekund od początku epoki
seconds = time.time()

# Aby uzyskać datę, musimy przekonwertować sekundy na datę
current_date = datetime.fromtimestamp(seconds)

# Wyświetlanie daty i czasu
print(current_date)
```
Wynik:
```
2021-11-22 13:39:12.678751
```

## Głębsze zagłębienie:
W Pythonie istnieje wiele modułów i funkcji do pracy z datami, ale najbardziej popularnym i zalecanym jest moduł ```datetime```. Jest on łatwy w użyciu, a jednocześnie oferuje wiele opcji i funkcjonalności. Istnieją również inne alternatywy, takie jak moduły ```time``` i ```calendar```, ale nie są one tak wszechstronne i nie oferują takiego samego poziomu precyzji co ```datetime```.

Warto również zauważyć, że pobrana bieżąca data jest zależna od strefy czasowej w której znajduje się urządzenie. Programiści powinni mieć to na uwadze, zwłaszcza w przypadku pracy z różnymi strefami czasowymi.

## Zobacz także:
- [Dokumentacja modułu datetime w Pythonie](https://docs.python.org/3/library/datetime.html)
- [Alternatywne moduły do pracy z datami](https://realpython.com/python-datetime/)
- [Poradnik wideo o pracy z datami w Pythonie](https://www.youtube.com/watch?v=eirjjyP2qcQ)