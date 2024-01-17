---
title:                "Usuwanie znaków pasujących do wzorca"
html_title:           "Ruby: Usuwanie znaków pasujących do wzorca"
simple_title:         "Usuwanie znaków pasujących do wzorca"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Usuwanie znaków pasujących do określonego wzorca jest częstym zadaniem dla programistów. Polega ono na wykryciu i usunięciu wszystkich wystąpień określonego wzorca w tekście. Jest to szczególnie przydatne, gdy chcemy usunąć zbędne znaki, np. spacje, z tekstu.

## Jak to zrobić:
```Ruby
# Przykładowy tekst
tekst = "To jest przykładowy tekst"

# Usunięcie spacji
tekst.delete(" ")

# Wynik: "Tojestprzykładowytekst"
```

```Ruby
# Innym sposobem jest użycie wyrażenia regularnego
# Przykładowy adres email
email = "example@email.com"

# Usunięcie znaku "@"
email.sub("@", "")

# Wynik: "exampleemail.com"
```

## Wnikliwe spojrzenie:
Usuwanie znaków pasujących do wzorca jest jednym z wielu sposobów na manipulowanie tekstem w Ruby. Początkowo, w starszych wersjach języka, wykonywało się to za pomocą metody `gsub`, która wymaga użycia wyrażeń regularnych. Jednak w nowszych wersjach języka dostępna jest również metoda `delete`, co znacznie upraszcza to zadanie.

Możliwe jest także wykorzystanie wyrażeń regularnych do zlokalizowania oraz zamiany określonych znaków w tekście. Jednak w przypadku gdy chcemy jedynie usunąć określone znaki, metoda `delete` jest wyraźnie bardziej odpowiednia.

## Zobacz także:
- [Dokumentacja Ruby o metodzie `delete`](https://ruby-doc.org/core-2.7.1/String.html#method-i-delete)
- [Wprowadzenie do wyrażeń regularnych w Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Inne przydatne metody do manipulowania tekstem w Ruby](https://www.rubyguides.com/2015/05/ruby-strings/)