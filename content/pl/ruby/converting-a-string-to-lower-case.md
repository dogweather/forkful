---
title:                "Konwersja ciągu znaków do małych liter"
html_title:           "Ruby: Konwersja ciągu znaków do małych liter"
simple_title:         "Konwersja ciągu znaków do małych liter"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Istnieje kilka powodów, dla których warto przekonwertować ciąg znaków na małe litery w języku Ruby. Pierwszym z nich jest poprawność danych, ponieważ wiele funkcji w Ruby oczekuje, że dane będą w jednym formacie, a zamiana na małe litery ułatwia porównywanie i przetwarzanie danych. Kolejnym powodem jest estetyka i czytelność kodu - korzystanie z jednego konwencjonalnego formatu liter jest przyjemniejsze dla oka i ułatwia czytanie kodu innym programistom.

## Jak to zrobić
Czasami wystarczy jedna prosta metoda, aby przekonwertować ciąg znaków na małe litery w języku Ruby. Wystarczy wywołać metodę `downcase` na zmiennej zawierającej łańcuch znaków. Zobaczmy to na przykładzie:

```Ruby
string = "PRzyKłAdOWy TEKST"
puts string.downcase
```

Output:
```
przykładowy tekst
```

Jeśli chcemy, aby zmiana była tymczasowa i nie chcemy zmieniać wartości zmiennej, możemy użyć `downcase!` z wykrzyknikiem na końcu, co spowoduje, że zmienimy oryginalny ciąg znaków. Możemy również przekazać dużą literę lub cały wyraz jako argument do metody, aby zamienić na małe litery tylko te części ciągu, które chcemy.

## Głębszy zanurzenie
Zanim przejdziemy do konkluzji, warto zauważyć, że zamiana ciągu znaków na małe litery w języku Ruby może przebiegać nieco inaczej, jeśli używamy znaków spoza alfabetu angielskiego lub specjalnych znaków. W tym przypadku zależy to od kodowania znaków, które używamy. Możemy także wykorzystać inne metody, takie jak `swapcase`, `upcase`, czy `capitalize`, aby manipulować wielkością liter w ciągu.

## Zobacz także
Jeśli chcesz dowiedzieć się więcej o konwersji i manipulacji ciągami znaków w języku Ruby, zapoznaj się z tymi pomocnymi materiałami:

- [Ruby String class documentation](https://ruby-doc.org/core-#{RUBY_VERSION}/String.html)
- [Ruby String methods guide](https://www.rubyguides.com/2015/05/ruby-string-methods/)
- [Ruby String Cookbook](https://learnbyexample.github.io/Ruby_String/#table-of-contents)

Dziękujemy za przeczytanie! Mam nadzieję, że ten krótki artykuł był dla Ciebie pomocny. Do zobaczenia w kolejnych!