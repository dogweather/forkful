---
title:                "Interpolacja ciągu znaków"
html_title:           "Ruby: Interpolacja ciągu znaków"
simple_title:         "Interpolacja ciągu znaków"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Interpolacja ciągu znaków to proces wstawiania wartości zmiennych do tekstu w celu utworzenia nowego ciągu znaków. Programiści często używają go do tworzenia dynamicznych wiadomości lub szablonów, które dostosowują się do zmiennej zawartości.

## Jak: 

```Ruby
name = "Jan"
age = 30

"Twoje imię to #{name} i masz #{age} lat."
```

Output:
`
"Twoje imię to Jan i masz 30 lat."
`

## Gleboka Nurkowanie:

Interpolacja ciągu znaków jest od dawna wykorzystywana w językach programowania, aby ułatwić tworzenie dynamicznych i interaktywnych aplikacji. Alternatywą dla interpolacji jest konkatenacja, czyli łączenie wielu ciągów znaków w jeden, ale jest to mniej wygodne i niepraktyczne w przypadku dłuższych tekstów. Implementacja interpolacji w Ruby jest prosta i przejrzysta dzięki użyciu symbolu `#{}`.

## Zobacz także:

- [Ruby String Interpolation](https://www.rubyguides.com/2016/04/ruby-string-interpolation/)
- [Ruby Documentation: String Interpolation](https://ruby-doc.org/core-2.7.0/doc/syntax/literals_rdoc.html#label-String+Interpolation)