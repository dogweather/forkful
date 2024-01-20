---
title:                "Obliczanie daty w przyszłości lub przeszłości"
html_title:           "Gleam: Obliczanie daty w przyszłości lub przeszłości"
simple_title:         "Obliczanie daty w przyszłości lub przeszłości"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Obliczanie daty w przyszłości lub przeszłości polega na dodawaniu lub odejmowaniu jednostek czasu od określonej daty. Programiści robią to podczas tworzenia aplikacji do zarządzania czasem, planowania itp.

## Jak to zrobić:
```Gleam
import gleam/time.{span_to_ms, new_span}

fn add_days_to_date(date: Time, days: Float) -> Time {
    let span = new_span(days: days)
    Time.add(date, span_to_ms(span))
}

fn subtract_days_from_date(date: Time, days: Float) -> Time {
    let negative_span = new_span(days: -days)
    Time.add(date, span_to_ms(negative_span))
}

// Przykładowe użycie

let current_date = Time.now()
let future_date = add_days_to_date(current_date, 15.0)
let past_date = subtract_days_from_date(current_date, 15.0)
```

## Dogłębne spojrzenie
Obliczanie daty w przyszłości i przeszłości jest powszechną praktyką data science i programowania. W historii technologii mogliśmy zobaczyć różne metody rozwiązania tego problemu, na przykład używanie czasu UNIX czy kodu Juliana. Współczesne języki programowania, takie jak Gleam, zapewniają wbudowane biblioteki i funkcje do obsługi dat i czasu, co ułatwia zadanie.

Alternatywą do powyższego kodu byłoby dodawanie lub odejmowanie milisekund bezpośrednio do lub od czasu, ale jest to mniej czytelne.

Szczegółem implementacyjnym jest to, że funkcje `add_days_to_date` i `subtract_days_from_date` używają funkcji `new_span` do tworzenia okresu czasu do dodania lub odjęcia, a następnie przeliczają ten okres na milisekundy za pomocą `span_to_ms` zanim dodadzą lub odejmą ten czas od daty.