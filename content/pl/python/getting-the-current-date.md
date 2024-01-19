---
title:                "Pobieranie aktualnej daty"
html_title:           "Arduino: Pobieranie aktualnej daty"
simple_title:         "Pobieranie aktualnej daty"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie bieżącej daty oznacza wydobycie aktualnej daty z systemu. Programiści robią to, gdy potrzebują zapisywać datę i godzinę zdarzeń lub generować raporty z datami.

## Jak to zrobić:

Możemy zrobić to przy użyciu wbudowanej biblioteki Python "datetime". Sprawdź poniższy kod:

```Python
import datetime

teraz = datetime.datetime.now()
print(teraz)
```

Po uruchomieniu kodu wyżej, wyświetli aktualny czas w formacie 'YYYY-MM-DD HH:MM:SS.ssssss'.

## Dogłębna analiza:

Historia: Pobieranie bieżącej daty to stara praktyka, która wywodzi się z tradycyjnych języków programowania, które miały wbudowane funkcje do pobierania daty systemowej.

Alternatywy: Oprócz `datetime`, istnieją inne moduły Pythona, takie jak `time` oraz `calendar`, które można wykorzystać do pracy z datami. Każdy z nich ma swoje unikalne funkcje i składnię.

Szczegóły implementacji: `datetime.datetime.now()` jest najprostszym sposobem pobrania daty z systemu. Działa to poprzez wywołanie systemowego zegara i pobranie z niego aktualnej daty i czasu.

## Zobacz także:

Aby dowiedzieć się więcej o pracy z datami w Pythonie, odwiedź poniższe linki:

1. Dokumentacja Python dla modułu `datetime`: https://docs.python.org/3/library/datetime.html
2. Dokumentacja Python dla modułu `time`: https://docs.python.org/3/library/time.html
3. Dokumentacja Python dla modułu `calendar`: https://docs.python.org/3/library/calendar.html