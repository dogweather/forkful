---
title:                "Python: Konwertowanie daty na ciąg znaków"
simple_title:         "Konwertowanie daty na ciąg znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest częstym zadaniem w programowaniu, szczególnie jeśli pracujesz z danymi czasowymi. Przekształcenie daty do postaci łatwej do odczytania przez człowieka jest nie tylko przydatne dla użytkowników, ale również ułatwia porównywanie i sortowanie danych.

## Jak to zrobić

Konwersja daty do ciągu znaków w języku Python jest dość prosta. Najpierw musimy zaimportować moduł `datetime`, który zawiera funkcje do pracy z datami. Następnie możemy skorzystać z funkcji `strftime()` aby przekonwertować datę na ciąg znaków według określonego formatu. Przykładowy kod wyglądałby następująco:

```Python
import datetime

# przypisanie aktualnej daty do zmiennej
now = datetime.datetime.now()

# skonwertowanie daty do ciągu znaków w formacie "dzień-miesiąc-rok"
data_w_ciągu_znaków = now.strftime("%d-%m-%Y")

# wyświetlenie wyniku
print("Data w postaci ciągu znaków:", data_w_ciągu_znaków)
```

Powyższy kod zwróci następujący wynik:

```
Data w postaci ciągu znaków: 25-05-2021
```

Możemy również określić wiele innych formatów, aby dopasować nasze potrzeby. Kilka przydatnych opcji znajdziesz w sekcji "Głębsze spojrzenie" poniżej.

## Głębsze spojrzenie

Funkcja `strftime()` pozwala na konwersję daty do różnych formatów, w tym m.in.:

- `%d`: dzień
- `%m`: miesiąc
- `%Y`: rok (czterocyfrowy)
- `%y`: rok (dwucyfrowy)
- `%H`: godzina (24-godzinny zapis)
- `%I`: godzina (12-godzinny zapis)
- `%M`: minuta
- `%S`: sekunda
- `%p`: AM/PM (tylko w przypadku 12-godzinnego zapisu)

Możemy również połączyć różne formaty, np. "%d-%m-%Y %H:%M:%S" zwróci nam datę w postaci "25-05-2021 14:33:45".

Warto również wspomnieć, że funkcja `strftime()` działa również z innymi obiektami daty, np. zmienną typu `date` lub `time`.

## Zobacz również

Oprócz powyższych informacji, możesz zapoznać się z dokumentacją modułu `datetime` w języku Python lub sprawdzić różne formaty dat na stronie [strftime.org](https://strftime.org/).