---
title:                "Wydrukowanie wyjścia debugowania"
html_title:           "Ruby: Wydrukowanie wyjścia debugowania"
simple_title:         "Wydrukowanie wyjścia debugowania"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/printing-debug-output.md"
---

{{< edit_this_page >}}

## Dlaczego

Czasami podczas pisania kodu w Rubym napotykamy na błędy lub problemy, które utrudniają nam znalezienie przyczyny niepoprawnego działania programu. Wykorzystanie wypisywania informacji debugowania może pomóc nam w szybkim rozwiązaniu tych problemów poprzez śledzenie kolejnych kroków wykonywanych przez program.

## Jak to zrobić

Aby wypisać informacje debugowania w Rubym, możemy wykorzystać metodę `puts` i przekazać jej zmienną lub wyrażenie, które chcemy wyświetlić. Możemy także wykorzystać specjalną metodę `p`, która wyświetli nam informacje debugowania w postaci czytelnego dla programisty zapisu.

```Ruby
# Wypisanie zmiennej do celów debugowania
x = 5
puts x # 5

# Wykorzystanie metody p
p "Hello World!" # "Hello World!"
```

Wykorzystanie metod `puts` i `p` pozwala nam na wypisanie wartości zmiennych oraz dokładne śledzenie kolejnych kroków wykonywanych przez program. Dodatkowo, możemy wykorzystać także argument `inspect` w metodzie `p` aby uzyskać szczegółowe informacje o obiekcie, które mogą być przydatne podczas debugowania.

## Dogłębna analiza

Istnieje również wiele innych metod i narzędzi, które pozwalają na wypisanie informacji debugowania w Rubym. Jednym z nich jest gem `pry`, który pozwala na interaktywne debugowanie naszego kodu oraz wyświetlanie wartości zmiennych w danym momencie wykonania programu.

Innym ciekawym narzędziem jest gem `byebug`, który pozwala na zatrzymywanie wykonania programu w określonych miejscach i przeprowadzanie interaktywnych inspekcji zmiennych.

Warto także zapoznać się z dokumentacją Rubiego, gdzie znajduje się więcej informacji na temat wypisywania informacji debugowania oraz dostępnych narzędzi.

## Zobacz także

- [Przykłady użycia metody `puts` w Rubym](https://www.rubyguides.com/2018/10/ruby-puts-method/)
- [Interaktywne debugowanie z wykorzystaniem gemu `pry`](https://www.sitepoint.com/pry-a-simple-start/)
- [Gem `byebug` w praktyce](https://www.petrbílek.cz/en/blog/gotta-stop-em-all-using-byebug/)