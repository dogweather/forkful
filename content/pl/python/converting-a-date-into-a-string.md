---
title:    "Python: Konwertowanie daty na ciąg znaków"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Zamiana daty na ciąg znaków jest bardzo przydatną umiejętnością w programowaniu. Po pierwsze, pozwala nam wyświetlić datę w czytelny sposób dla użytkownika. Ponadto, może być niezbędna przy pracy z bazami danych lub zewnętrznymi API, które wymagają formatu daty jako ciągu znaków.

## Jak to zrobić

```Python
import datetime

# przykładowa data
data = datetime.date(2021, 2, 5)

# przekształcenie daty na ciąg znaków w formacie YYYY-MM-DD
data_str = data.strftime("%Y-%m-%d")

print(data_str) # output: "2021-02-05"

# możemy również zmienić format daty, na przykład na DD-MM-YYYY
data_str = data.strftime("%d-%m-%Y")
print(data_str) # output: "05-02-2021"
```

W powyższym przykładzie użyliśmy metody `strftime()` z modułu datetime. Pierwszy argument tej metody to format, według którego chcemy przekształcić datę na ciąg znaków. Oznaczenia, które możemy użyć w formacie, są dostępne w dokumentacji Pythona.

## Głębszy przegląd

Podczas przekształcania daty na ciąg znaków, ważne jest, aby pamiętać o używaniu odpowiednich oznaczeń, w przeciwnym razie format daty może być niepoprawny. Na przykład, jeśli chcemy wyświetlić miesiąc w formacie od `01` do `12`, należy użyć oznaczenia `%m`, a nie `%M`. Przekonwertowanie daty na ciąg znaków jest również dostępne z użyciem metody `str()`, jednak nie pozwala na dostosowanie formatu.

## Zobacz także

- Dokumentacja Pythona: https://docs.python.org/3/library/datetime.html#strftime-strptime-behavior
- Przekształcanie daty i czasu w Pythonie: https://realpython.com/python-datetime/
- Wykorzystanie dat w programowaniu: https://progracraft.com/how-to-do-dates-and-times-in-python-1-basic-date-time-operations/