---
title:                "Python: Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Wyliczanie daty w przyszłości lub przeszłości może być niezbędne w wielu projektach programistycznych. W przypadku tworzenia aplikacji zorientowanych na czas, takich jak aplikacje kalendarza lub rezerwacyjne, konieczne jest wyliczanie dat w przyszłości lub przeszłości. Ponadto, posiadanie umiejętności wyliczania dat może pomóc w rozwiązaniu wielu problemów, takich jak obliczanie wieku lub daty ważności.

## Jak to zrobić

Aby wyliczyć datę w przyszłości lub przeszłości w języku Python, możemy skorzystać z modułu `datetime`. Najpierw musimy zaimportować ten moduł do naszego programu, a następnie możemy wykorzystać różne funkcje, aby wyliczyć odpowiednią datę.

```Python
# importowanie modułu datetime
import datetime

# wyliczenie daty jutra
jutro = datetime.date.today() + datetime.timedelta(days=1)

# wyliczenie daty 30 dni temu
trzydzienki_temu = datetime.date.today() - datetime.timedelta(days=30)

# wydrukowanie wyników
print("Jutro będzie:", jutro)
print("30 dni temu było:", trzydziesci_temu)
```

W powyższym przykładzie wykorzystaliśmy funkcję `timedelta` do wyliczenia daty w przyszłości lub przeszłości. Możemy również użyć innych funkcji, takich jak `datetime.date.replace`, aby zmienić poszczególne elementy daty, na przykład rok lub miesiąc.

## Wnikliwe zagłębienie

Moduł `datetime` oferuje wiele funkcji i metod, które mogą być przydatne podczas wyliczania dat w przyszłości lub przeszłości. Możemy wykorzystać funkcje, takie jak `datetime.date.weekday`, aby uzyskać informację o dniu tygodnia lub `datetime.date.isoweekday`, aby uzyskać informację o dniu tygodnia w formacie ISO.

Ponadto, moduł `datetime` pozwala również na wyliczanie dat w różnych strefach czasowych i konwertowanie daty do różnych formatów. Warto więc bliżej przyjrzeć się temu modułowi i poznać jego możliwości.

## Zobacz także

- Dokumentacja modułu `datetime`: https://docs.python.org/3/library/datetime.html
- Przewodnik po obliczaniu dat w Pythonie: https://stackabuse.com/how-to-calculate-dates-in-python/
- Wideo na temat wyliczania dat w przyszłości lub przeszłości: https://www.youtube.com/watch?v=8yFHGC4rF8g