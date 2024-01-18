---
title:                "Parsowanie daty z ciągu znaków"
html_title:           "Ruby: Parsowanie daty z ciągu znaków"
simple_title:         "Parsowanie daty z ciągu znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Parsowanie daty z ciągu tekstowego to proces, w którym programista przekształca datę wyrażoną jako ciąg znaków w określonym formacie do obiektu daty, co ułatwia jej dalsze manipulacje. Programiści stosują to narzędzie, aby uprościć operacje z datami i uniknąć błędów w obliczeniach.

## Jak to zrobić:
Poniżej znajdują się przykłady kodu w języku Ruby, które pokazują, jak parsować datę z ciągu tekstowego:

```ruby
Date.strptime("11/30/2021", "%m/%d/%Y")
#=> #<Date: 2021-11-30 ((2459595j,0s,0n),+0s,2299161j)>
````
Ta metoda ```strptime``` przyjmuje dwa argumenty - ciąg tekstowy z datą oraz format, w jakim ta data jest przedstawiana. Można użyć wielu różnych symboli, aby precyzyjnie zdefiniować format daty.

Inny przykład:

```ruby
Date.strptime("2021/11/30", "%Y/%m/%d")
#=> #<Date: 2021-11-30 ((2459595j,0s,0n),+0s,2299161j)>
```

Oba powyższe przykłady zwracają ten sam obiekt daty, ponieważ formaty są zgodne. Jednak jeśli format byłby inny, wynik również byłby inny.

## Głębszy wgląd:
Parsowanie daty z ciągu tekstowego jest popularną funkcjonalnością, która pozwala programistom pracować z datami w prosty i dokładny sposób. W przeszłości, zanim powstały nowoczesne biblioteki do obsługi dat, parsowanie dat było często uciążliwe i wymagające dużo kodu. Obecnie, programiści mogą wybierać spośród różnych narzędzi, takich jak biblioteki ActiveSupport lub DateTime, które ułatwiają pracę z datami.

## Zobacz również:
- [Ruby dokumentacja - metoda ```strptime```](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html#method-c-strptime)
- [Biblioteka ActiveSupport](https://guides.rubyonrails.org/active_support_core_extensions.html)
- [Biblioteka DateTime](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)