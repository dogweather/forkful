---
title:                "Przetwarzanie daty z ciągu znaków"
html_title:           "Python: Przetwarzanie daty z ciągu znaków"
simple_title:         "Przetwarzanie daty z ciągu znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Czego i Dlaczego?

Sparsowanie daty ze stringa to proces przekształcania tekstu reprezentującego datę na obiekt daty, który może być wykorzystany w programie. Programiści wykonują tę operację głównie w celu odczytania i manipulacji danymi datowymi.

## Jak to Zrobić?

```Python
from datetime import datetime

date_string = "20/01/2020"
parsed_date = datetime.strptime(date_string, "%d/%m/%Y")
print(parsed_date)

# Output: 2020-01-20 00:00:00
```

W powyższym przykładzie wykorzystano moduł ```datetime``` oraz metodę ```strptime()```, która odczytuje format daty i zwraca obiekt daty. Należy pamiętać, że format daty musi być dokładnie zgodny z formatem w stringu, w przeciwnym razie pojawi się błąd.

## Głębsze Zagadnienia

Sparsowanie dat z stringa jest problemem, który pojawił się wraz z rozwojem programowania komputerowego. Wcześniej daty były przechowywane w postaci liczb, a programiści musieli sami zamieniać je na odpowiedni format. Alternatywnym sposobem jest użycie modułu ```re``` i wyrażenia regularnego do wyszukania dat w tekście. Implementacja parsowania dat zależy od języka programowania i dostępnych bibliotek.

## Zobacz Również

Więcej informacji na temat parsowania dat w Pythonie można znaleźć w dokumentacji: https://docs.python.org/3/library/datetime.html.

Jeśli interesuje Cię temat dat i czasu w programowaniu, polecamy również skorzystać z biblioteki ```arrow```, która zawiera szereg przydatnych funkcji: https://arrow.readthedocs.io/en/latest/.