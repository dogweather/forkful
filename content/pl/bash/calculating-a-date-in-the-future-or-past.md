---
title:    "Bash: Obliczanie daty w przyszłości lub przeszłości"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/bash/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasem w życiu potrzebujemy uwzględnić daty w przyszłości lub w przeszłości, zwłaszcza w kontekście programowania. Na przykład, potrzebujemy wyliczyć datę ważności ważnego dokumentu lub zaplanować zadania do wykonania w przyszłych dniach. Warto więc wiedzieć, jak obliczać daty w Bashu.

## Jak to zrobić

Aby obliczyć datę w przyszłości lub w przeszłości w Bashu, użyjemy komendy `date`. Składnia jest następująca:

```Bash
date -d "DATA OPERACJA LICZBOWA I JEDNOSTKA CZASU"
```

Gdzie:
- "DATA" to data odniesienia, czyli punkt, względem którego będziemy wyliczać datę.
- "OPERACJA LICZBOWA" to liczba dni, miesięcy lub lat, którą chcemy dodać lub odjąć od daty odniesienia.
- "JEDNOSTKA CZASU" to jednostka, w której wyrażona jest liczba (d - dni, m - miesiące, y - lata).

Przykłady:

Obliczanie daty w przyszłości:

```Bash
date -d "now 5 days"
```
Output: Thu Aug 12 22:24:22 CEST 2021

Obliczanie daty w przeszłości:

```Bash
date -d "2021-08-01 2 months 4 days"
```

Output: Fri Jun 11 22:26:34 CEST 2021

Kod jest prosty i intuicyjny, warto jednak przetestować i zapoznać się z dokumentacją komendy `date`, aby poznać więcej opcji i możliwości.

## Głębsza analiza

Komenda `date` jest częścią gnu coreutils, czyli zestawu narzędzi systemowych w systemach GNU/Linux. Oznacza to, że jest ona dostępna na wielu systemach operacyjnych i jest zgodna z ogólnie przyjętym standardem. Ponadto, komenda ta posiada wiele opcji, pozwalających na wyświetlanie daty w różnych formatach oraz dokładne wyliczenia czasu pomiędzy datami.

Warto również wspomnieć, że Bash jest jednym z najpopularniejszych języków skryptowych, używanym przede wszystkim w systemach Linux i Unix. Poznanie jego możliwości z pewnością przyspieszy i ułatwi pracę z tymi systemami.

## Zobacz też

- Dokumentacja komendy `date`: https://www.gnu.org/software/coreutils/manual/html_node/date-invocation.html
- Lista jednostek czasu w komendzie `date`: https://www.gnu.org/software/coreutils/manual/html_node/Time-conversion-specifiers.html
- Przykłady użycia komendy `date`: https://www.cyberciti.biz/faq/linux-unix-get-yesterdays-tomorrows-date/