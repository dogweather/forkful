---
title:                "Obliczanie daty w przyszłości lub przeszłości"
aliases:
- /pl/vba/calculating-a-date-in-the-future-or-past/
date:                  2024-02-01T21:49:31.039455-07:00
model:                 gpt-4-0125-preview
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/vba/calculating-a-date-in-the-future-or-past.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości polega na określeniu daty, która jest określoną liczbę dni, miesięcy lub lat dalej od danej daty. Programiści często potrzebują tej funkcjonalności do automatyzowania przypomnień, subskrypcji, dat wygaśnięcia i planowania zadań w różnych aplikacjach.

## Jak to zrobić:
W Visual Basic for Applications (VBA) podstawową funkcją używaną do obliczania dat w przyszłości lub przeszłości jest `DateAdd()`. Funkcja ta dodaje określony interwał czasu do daty, zwracając nową datę.

Oto podstawowy przykład dodania 10 dni do bieżącej daty:

```vb
Dim futureDate As Date
futureDate = DateAdd("d", 10, Date) ' Dodaje 10 dni do bieżącej daty
Debug.Print futureDate ' Wypisuje coś w rodzaju: 04/20/2023
```

Podobnie, aby znaleźć datę 10 dni w przeszłości:

```vb
Dim pastDate As Date
pastDate = DateAdd("d", -10, Date) ' Odejmuje 10 dni od bieżącej daty
Debug.Print pastDate ' Wypisuje: 03/31/2023, zakładając, że dzisiaj jest 04/10/2023
```

Te przykłady są dość proste. Możesz zastąpić `"d"` innymi kodami interwałów, takimi jak `"m"` dla miesięcy i `"yyyy"` dla lat, aby obliczyć różne rodzaje kalkulacji daty. Oto jak można by obliczyć datę rok w przyszłości:

```vb
Dim nextYear As Date
nextYear = DateAdd("yyyy", 1, Date) ' Dodaje 1 rok do bieżącej daty
Debug.Print nextYear ' Wypisuje: 04/10/2024, jeśli dziś jest 04/10/2023
```

## Szczegółowa analiza
Funkcja `DateAdd` jest fundamentalną częścią VBA od momentu jej powstania, wywodząc się z jej poprzednika BASIC. Chociaż oferuje prostotę dodawania lub odejmowania interwałów czasu od dat, ważne jest zauważenie, że VBA, w tym jego funkcje obsługi dat, może nie zawsze dorównać wygodzie lub efektywności znalezionej w nowszych językach programowania.

Na przykład nowoczesne języki takie jak Python z modułem `datetime` lub JavaScript z bibliotekami takimi jak `moment.js` i `date-fns` zapewniają bardziej intuicyjne i potężne sposoby na manipulację datami. Te opcje oferują lepsze wsparcie dla lokalizacji, stref czasowych i lat przestępnych, co może czynić je bardziej odpowiednimi dla aplikacji wymagających precyzyjnych obliczeń dat na skalę globalną.

Jednakże, dla makr Excela i aplikacji wymagających integracji w ekosystemie Microsoft Office, VBA pozostaje praktycznym wyborem. Prostota w bezpośrednim dostępie i manipulacji danych Excela jest znaczącą zaletą. Co więcej, dla większości podstawowych obliczeń dat, takich jak planowanie i przypomnienia, `DateAdd()` w VBA zapewnia odpowiednie i proste rozwiązanie. Jego składnia jest łatwa do opanowania dla nowicjuszy, a integracja z szerszymi aplikacjami pakietu Office zapewnia jego aktualność w konkretnych przypadkach użycia.

Podsumowując, chociaż alternatywne języki programowania mogą oferować nowocześniejsze podejścia do obliczania dat, `DateAdd()` w VBA stanowi świadectwo trwałości tego języka w dziedzinach, gdzie jest on najbardziej potrzebny.
