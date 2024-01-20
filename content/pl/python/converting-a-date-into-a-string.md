---
title:                "Konwersja daty na ciąg znaków"
html_title:           "Clojure: Konwersja daty na ciąg znaków"
simple_title:         "Konwersja daty na ciąg znaków"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Konwersja daty na łańcuch znaków (string) to proces przekształcenia daty (nieco trudnego do obsługi formatu) w tekst łatwiejszy do czytania i zrozumienia. Programiści robią to, żeby łatwiej manipulować danymi daty, np. wyświetlać je w określonym formacie.

## Jak to zrobić:

Załóżmy, że masz datę w formacie 'datetime'. Poniżej znajdziesz sposób, jak przekształcić ją na napis w formacie 'YYYY-MM-DD'.

```Python
from datetime import datetime

# stworzenie daty
date = datetime.now()

# konwersja daty na string
date_string = date.strftime('%Y-%m-%d')

print(date_string)
```
W efekcie otrzymasz na wyjściu dzisiejszą datę w formacie 'YYYY-MM-DD', np. '2022-01-22'.

## Szersze spojrzenie

Historia: W dawnych czasach, kiedy pamięć komputera była bardzo droga, daty często przechowywano jako ciągi znaków, aby zaoszczędzić miejsce. 

Alternatywy: Istnieją różne alternatywy dla powyższego przykładu, np. można użyć funkcji `isoformat()` zamiast `strftime()`. Wybór zależy od tego, co chcesz osiągnąć.

Szczegóły implementacji: Metoda `strftime()` jest częścią standardowej biblioteki Pythona i może być używana do konwertowania dat na napisy w wielu innych formatach, nie tylko 'YYYY-MM-DD'. Więcej na temat dostępnych formatów można znaleźć w dokumentacji Pythona.

## Zobacz także

1. Dokumentacja Python dla `strftime()` i `strptime()`: https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior

2. Dokumentacja Python dla `isoformat()`: https://docs.python.org/3/library/datetime.html#datetime.date.isoformat

Więcej na ten temat można znaleźć w odpowiednich artykułach i kursach Pythona. Praca z datami może być skomplikowana, ale Python oferuje wiele narzędzi, które ułatwiają to zadanie.