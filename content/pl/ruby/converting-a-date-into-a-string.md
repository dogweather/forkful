---
title:                "Konwersja daty na ciąg znaków."
html_title:           "Ruby: Konwersja daty na ciąg znaków."
simple_title:         "Konwersja daty na ciąg znaków."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Konwersja daty na ciąg znaków jest często niezbędnym krokiem w programowaniu. Umożliwia to wyświetlenie daty w czytelnej formie lub jej wykorzystanie w innych operacjach.

## Jak to zrobić

Aby skonwertować datę na ciąg znaków w Ruby, możesz skorzystać z metody `strftime()`. Przyjmuje ona formatowanie daty w postaci łańcucha znaków i zwraca ciąg znaków zawierający datę w wybranej formie. Na przykład:

```Ruby
date = Time.new(2021, 12, 1)
date_str = date.strftime("%d/%m/%Y")
puts date_str # wyświetli 01/12/2021
```

Możesz także wykorzystać gotowe formaty dostępne w języku Ruby, jak na przykład `iso8601` lub `rfc2822`.

```Ruby
date = Time.new(2021, 12, 1)
date_str = date.iso8601
puts date_str # wyświetli 2021-12-01T00:00:00+00:00
```

## Głębszy wgląd

W Ruby daty są przechowywane jako obiekty klasy `Time`. W przypadku metody `strftime()` możesz wykorzystać wiele różnych znaków, aby sformatować datę według swoich potrzeb.

Na przykład, `%a` zwróci skrócony dzień tygodnia (np. "Mon"), a `%b` skrócony miesiąc (np. "Jan"). Możesz także wykorzystać `%d` do wyświetlenia dnia miesiąca w formacie z zerem wiodącym (np. "01") lub `%Y` do wyświetlenia roku w formacie czterocyfrowym (np. "2021").

Możesz również wykorzystać `%H` do wyświetlenia godziny w formacie 24-godzinnym lub `%M` do wyświetlenia minuty. Pełną listę możliwych znaków formatujących znajdziesz w dokumentacji języka Ruby.

## Zobacz także

- Dokumentacja języka Ruby: https://ruby-doc.org/core-3.0.0/Time.html#method-i-strftime
- Jak sformatować datę i czas w Ruby: https://www.rubyguides.com/2015/09/ruby-time-format/
- Poradnik dla początkujących: https://rubyguides.com/learn/ruby-string-formatting/