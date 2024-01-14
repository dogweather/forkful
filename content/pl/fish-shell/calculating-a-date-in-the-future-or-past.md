---
title:                "Fish Shell: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Dlaczego

Jeśli jesteś programistą lub po prostu lubisz używać nowych narzędzi, z pewnością zainteresuje Cię możliwość obliczenia daty w przyszłości lub przeszłości za pomocą powłoki Fish Shell. Pozwala to na łatwe i precyzyjne manipulowanie datami w różnych projektach.

# Jak to zrobić

Aby obliczyć datę w przyszłości lub przeszłości, należy użyć wbudowanej funkcji `date` w powłoce Fish Shell. Możesz podać liczbę określającą wybrany przedział czasowy, na przykład:

```Fish Shell
date -d "3 days"
```
 
To spowoduje wyświetlenie daty trzy dni od teraz. Możesz również użyć jednostek czasu, takich jak "weeks", "months" czy "years". Aby obliczyć datę w przeszłości, wystarczy dodać przed określeniem przedziału znak minus, np. `-1 week`. Możesz także dodawać i odejmować różne jednostki czasu w ramach jednej komendy, na przykład:

```Fish Shell
date -d "1 month 2 weeks"
```

To wskaże datę, która jest dokładnie 1 miesiąc i 2 tygodnie od teraz.

# Głębszy zanurzenie

Funkcja `date` w powłoce Fish Shell oferuje szeroki zakres opcji, które można użyć do dokładniejszego manipulowania datami. Możesz na przykład ustawić specyficzną datę, podając rok, miesiąc i dzień jako argumenty:

```Fish Shell
date -s "2020-12-01"
```

Powyższa komenda ustawia datę na 1 grudnia 2020 roku. Możesz także formatować wyświetlanie daty za pomocą flagi `-f`, na przykład:

```Fish Shell
date -f "%Y/%m/%d" -d "2 weeks"
```

Ten przykład wyświetli datę dwóch tygodni od teraz w formacie "rok/miesiąc/dzień".

# Zobacz też

- Oficjalna dokumentacja powłoki Fish Shell dotycząca obliczania dat: https://fishshell.com/docs/current/cmds/date.html
- Przykładowa strona internetowa wykorzystująca funkcję `date`: https://bikramaditya.netlify.app/