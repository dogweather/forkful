---
title:                "Python: Porównywanie dwóch dat"
simple_title:         "Porównywanie dwóch dat"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Dlaczego

Porównywanie dat jest niezbędnym elementem programowania w Pythonie, ponieważ często musimy sprawdzić, czy jedna data jest wcześniejsza lub późniejsza od drugiej. Jest to szczególnie ważne w przypadku skryptów, które operują na danych z różnych okresów czasu, takich jak raporty finansowe czy analizy rynkowe.

## Jak to zrobić

Istnieją różne sposoby na porównywanie dat w Pythonie, a jednym z najprostszych jest użycie funkcji `datetime` i operatorów porównania. Przykładowy kod może wyglądać następująco:

```Python
from datetime import datetime

data_1 = datetime(2021, 5, 5)
data_2 = datetime(2021, 5, 10)

if data_1 < data_2:
   print("Data 1 jest wcześniejsza niż data 2")
elif data_1 > data_2:
   print("Data 1 jest późniejsza niż data 2")
else:
   print("Obie daty są takie same")
```

Powyższy kod definiuje dwie zmienne typu `datetime` i następnie porównuje je za pomocą operatorów `<`, `>` i `==`, które zwracają odpowiednie wartości logiczne. W tym przypadku wydrukowany zostanie komunikat "Data 1 jest wcześniejsza niż data 2".

## Głębsze zagłębienie

Przeprowadzając porównanie dat, musimy pamiętać o różnicach między datą a datą i czasem. Data jest reprezentowana przez klasę `date`, która zawiera tylko informacje o dniu, miesiącu i roku. Natomiast data i czas są reprezentowane przez klasę `datetime`, która dodatkowo przechowuje informacje o godzinie i minucie. Dlatego też przy porównywaniu dat i czasów należy używać odpowiednich klas, aby uniknąć nieporozumień.

Ponadto, istnieją funkcje wbudowane w Pythona, takie jak `timedelta`, które pozwalają na obliczanie różnicy między dwiema datami. Przykładowo, możemy sprawdzić ile dni minęło od dnia dzisiejszego do dnia urodzin osoby, używając poniższego kodu:

```Python
from datetime import date, datetime, timedelta

dzis = date.today()
urodziny = date(1990, 10, 24)

roznica = dzis - urodziny

print("Minęło już", roznica.days, "dni od Twoich urodzin")
```

## Zobacz także

- Dokumentacja Pythona o datach i czasie: https://docs.python.org/pl/3/library/datetime.html
- Porównywanie danych i czasów w Pythonie: https://www.programiz.com/python-programming/datetime/compare-dates
- Poradnik Python do porównywania dat i czasów: https://realpython.com/python-datetime/