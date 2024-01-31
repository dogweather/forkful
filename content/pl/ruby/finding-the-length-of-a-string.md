---
title:                "Znalezienie długości ciągu znaków"
date:                  2024-01-20T17:48:07.614786-07:00
model:                 gpt-4-1106-preview
simple_title:         "Znalezienie długości ciągu znaków"

category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Co i dlaczego?)
Znalezienie długości łańcucha znaków to po prostu określenie, ile znaków zawiera. Programiści robią to, aby weryfikować dane wejściowe, ograniczać tekst, czy też po prostu manipulować danymi tekstowymi.

## How to: (Jak to zrobić:)
```ruby
# Kod w Ruby do znalezienia długości stringa
text = "Witaj, świecie!"
puts text.length  # Wyświetla długość stringa

# Przykładowe wyjście: 15
```

Inny sposób to używanie aliasu `size`:
```ruby
puts text.size  # To samo co length, tez zwróci 15
```

## Deep Dive (Dogłębna analiza)
Długość stringa w Ruby jest bardzo prosta do uzyskania dzięki metodzie `length` lub jej aliasowi `size`, dostępnych od początków języka. Są to metody wbudowane w klasę `String`, co oznacza, że każdy obiekt typu string dziedziczy te metody.

Zanim metody te stały się standardem, mogłeś spotkać różne inne techniki, jak iteracja przez string do zliczenia znaków. Teraz to już niepotrzebnie skomplikowane i rzadko używane, ale to ciekawy przykład na zrozumienie działania pętli. Ważnym aspektem jest, że `length` zwraca liczbę znaków, a nie bajtów, co może być różnicą w tekstach zawierających wielobajtowe znaki, jak polskie litery.

W nowszych wersjach Ruby, `length` i `size` są szybkie i wydajne, ponieważ informacja o długości stringa jest przechowywana w obiekcie `String` i nie wymaga przetwarzania każdorazowo przy wywołaniu metody.

## See Also (Zobacz też)
- [Ruby Docs for String](https://ruby-doc.org/core-3.0.0/String.html) - oficjalna dokumentacja klasy `String` w Ruby.
- [Ruby Style Guide](https://rubystyle.guide/#strings) - zalecenia dotyczące stylu pracy ze stringami w Ruby.
- [Learn Ruby the Hard Way](https://learnrubythehardway.org/book/ex6.html) - tutorial zawierający praktyczne ćwiczenia ze stringami.
