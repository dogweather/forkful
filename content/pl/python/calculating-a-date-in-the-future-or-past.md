---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Python: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Obliczanie daty w przyszłości lub przeszłości polega na dodawaniu lub odejmowaniu określonej liczby dni do lub od danej daty. Programiści robią to, aby manipulować danymi czasu, np. do ustalania terminów wyznaczonych lub do analizy danych historycznych.

## Jak to zrobić:

Wykorzystamy moduł `datetime` w Pythonie. Przykładowo, aby obliczyć datę 5 dni od dzisiaj:

```Python
import datetime

obecna_data = datetime.date.today()
data_przyszla = obecna_data + datetime.timedelta(days=5)

print("Obecna data :", obecna_data)
print("Data po 5 dniach :", data_przyszla)
```

Uruchomienie powyższego kodu zwróci coś w stylu:

```
Obecna data : 2022-10-04
Data po 5 dniach : 2022-10-09
```

## Pod kątem szczegółowym:

1. **Kontekst historyczny**: Moduł `datetime` jest częścią standardowej biblioteki Pythona i jest dostępny od wersji 2.3, wydanej w lipcu 2003 roku.
2. **Alternatywy**: Istnieje wiele bibliotek Pythona, które oferują dodatkowe funkcje do manipulowania datami, takie jak `dateutil`, `pytz`, czy `Arrow`.
3. **Szczegóły implementacji**: Obiekt `timedelta` z modułu `datetime` reprezentuje różnicę pomiędzy dwoma datami lub czasami. Dodając lub odejmując obiekt `timedelta` do obiektu `date` lub `datetime`, możemy łatwo obliczyć daty w przyszłości lub przeszłości.

## Zobacz również:

1. [Dokumentacja Pythona dla modułu `datetime`](https://docs.python.org/3/library/datetime.html)
2. [`dateutil` - rozszerzenie biblioteki `datetime`](https://dateutil.readthedocs.io/en/stable/)
3. [Biblioteka `Arrow` - lepsza obsługa dat i czasu](https://arrow.readthedocs.io/en/latest/)