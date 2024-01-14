---
title:    "Ruby: Używając wyrażeń regularnych"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego korzystać z wyrażeń regularnych?

Wyrażenia regularne są narzędziem, które ułatwiają odnajdywanie i manipulowanie tekstem w sposób precyzyjny. Dzięki nim programiści mogą znacznie szybciej i dokładniej przetwarzać dane, co przekłada się na wydajniejsze programy. Pozwalają one na wyszukiwanie wzorców w tekście, a także na podmianę lub wycinanie wybranych części. Dzięki temu są niezwykle przydatnym narzędziem dla każdego programisty.

## Jak używać wyrażeń regularnych?

Aby rozpocząć pracę z wyrażeniami regularnymi, należy w pierwszej kolejności zaimportować bibliotekę do obsługi wyrażeń regularnych w języku Ruby. W przykładach poniżej użyto skrótu "re" dla nazwy biblioteki. Następnie, przy użyciu metod zawartych w bibliotece, można wyszukiwać, wycinać lub zamieniać tekst według określonych wzorców.

```Ruby
# Szukanie słowa "Ruby" w tekście
re = /Ruby/
text = "Jestem wielkim fanem języka Ruby!"
result = re.match(text)
puts result # wyświetli: "Ruby"

# Zamiana liczby zapisanej cyframi arabskimi na słowną
re = /(\d+)/
text = "Mam 25 lat."
result = text.gsub(re, "dwadzieścia pięć")
puts result # wyświetli: "Mam dwadzieścia pięć lat."
```

## Głębsze spojrzenie na wyrażenia regularne

Wyrażenia regularne w języku Ruby są bardzo zaawansowane i oferują wiele opcji, które mogą ułatwić pracę z tekstem. Przykładowo, można używać tzw. metaznaków, czyli specjalnych symboli, które określają różne znaki lub grupy znaków. Istnieje także możliwość ustawiania flag, które modyfikują sposób działania wyrażenia regularnego. Warto także zwrócić uwagę na operacje wyrażeń regularnych, takie jak alternacja czy kwantyfikatory, które pozwalają na jeszcze dokładniejsze dopasowanie wzorców.

## Zobacz także

- [Dokumentacja biblioteki wyrażeń regularnych w Ruby](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Kurs wyrażeń regularnych w Ruby na platformie Codecademy](https://www.codecademy.com/learn/learn-regular-expressions)
- [Narzędzie do testowania wyrażeń regularnych online - Rubular](https://rubular.com)