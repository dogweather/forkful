---
title:                "Analiza składniowa daty z ciągu znaków"
html_title:           "Clojure: Analiza składniowa daty z ciągu znaków"
simple_title:         "Analiza składniowa daty z ciągu znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Analiza daty z ciągu znaków to proces przekształcania tekstu w formacie daty (na przykład "13 grudnia 2021") na dane, które Python może przeczytać jako datę. Robimy to, aby ułatwić manipulowanie danymi daty i czasu, sortowanie, porównywanie itd.

## Jak to zrobić:

W Pythonie istnieje build-in moduł o nazwie `datetime` do obsługi dat i czasu. Możemy użyć metody `strptime` z tego modułu do analizy daty z ciągu znaków.

```Python
from datetime import datetime

data_string = '13 grudnia 2021'
data = datetime.strptime(data_string, '%d %B %Y')

print(data)
```

Po uruchomieniu tego kodu, otrzymamy:

```Python
2021-12-13 00:00:00
```

Po prostu przekazujemy nasz ciąg daty i format daty do metody `strptime` i voilà! Mamy naszą datę.

## Deep Dive:

Python oferuje wiele podejść do analizy dat, właściwie wyboru która ścieżka do podążania zależy od specyficznych wymagań projektu. Oprócz `datetime`, inne popularne moduły to `dateutil` i `arrow`, które oferują większą elastyczność i łatwość użycia, choć mogą wymagać dodatkowej instalacji.

Podczas pracy z `datetime`, ważne jest, aby dokładnie zrozumieć, jak działa formatowanie daty i czasu Pythona. Chociaż to nie jest skomplikowane, różne formaty mogą być mylące. Na przykład `%d` oznacza dzień miesiąca jako liczbę, a `%B` oznacza pełną nazwę miesiąca. Więcej informacji na temat formatowania można znaleźć w [dokumentacji Pythona](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes).

## Zobacz też:

1. Dokumentacja Pythona na temat `datetime`: https://docs.python.org/3/library/datetime.html
2. Dokumentacja Pythona na temat formatowania czasu: https://docs.python.org/3/library/datetime.html#strftime-and-strptime-format-codes
3. Moduł `dateutil`: https://dateutil.readthedocs.io/en/stable/
4. Moduł `arrow`: https://arrow.readthedocs.io/en/latest/