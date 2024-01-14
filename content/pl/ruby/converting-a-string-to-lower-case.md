---
title:                "Ruby: Zmiana ciągu znaków na małe litery"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się dlaczego w programowaniu tak często trzeba zmieniać wielkość liter w łańcuchach znaków? Przykładowo, gdy potrzebujemy porównać dwa teksty, musimy upewnić się, że zostały wpisane dokładnie tak samo, nawet jeśli użytkownik przypadkowo użyje małych lub wielkich liter. Dlatego konwersja łańcuchów znaków na tylko jedną wielkość liter jest bardzo ważnym elementem programowania i jest nieodzowna w wielu projektach.

## Jak to zrobić

Jeśli potrzebujesz zmienić wszystkie litery w stringu na małe, możesz skorzystać z metody `downcase`. Przykład kodu w Ruby wygląda następująco:

```Ruby
string = "PROGRAMOWANIE"
puts string.downcase
```

W tym przykładzie używam zmiennej `string` z wartością "PROGRAMOWANIE". Metoda `downcase` zmieni wszystkie litery na małe, więc wynikiem wyświetlonym na ekranie będzie "programowanie".

Jeśli chcesz zmienić tylko pierwszą literę w tekście na małą, możesz użyć metody `capitalize`. Przykład przy użyciu tego samego stringu wyglądałby tak:

```Ruby
puts string.capitalize
# Wynik: Programowanie
```

Możesz też użyć metody `swapcase`, która zamieni wszystkie małe litery na duże i odwrotnie. Przykład:

```Ruby
string = "Programowanie w RuBy"
puts string.swapcase
# Wynik: pROGRAMOWANIE W rUbY
```

## Głębsze zagadnienia

Wszystkie wymienione metody (`downcase`, `capitalize` i `swapcase`) zmieniają wielkość liter tylko na poziomie znaków alfabetu łacińskiego. Jeśli użyjesz ich na znakach spoza tego zakresu, nie wywołają one żadnych zmian. Na przykład, w języku polskim znaki diakrytyczne, takie jak "ą", "ś" czy "ł" nie są zamieniane na małe odpowiedniki przez metody `downcase` czy `capitalize`.

W takich przypadkach, warto sięgnąć po metodę `downcase` z parametrem `Unicode`, która będzie działać na znakach spoza zakresu ASCII. Przykład:

```Ruby
string = "Życie jest piękNe"
puts string.downcase(:unicode)
# Wynik: życie jest piękne
```

## Zobacz też

- [Dokumentacja Ruby - String](https://ruby-doc.org/core-2.7.2/String.html)
- [Metody edycji łańcuchów znaków w Ruby](https://www.youtube.com/watch?v=WF8JkiMIXpE)
- [Tutorial: Szczegółowe omówienie metod dla łańcuchów znaków w Ruby](https://www.rubyguides.com/2018/04/string-methods/)

Dzięki temu przewodnikowi powinieneś mieć podstawową wiedzę na temat konwersji łańcuchów znaków na małe litery w Ruby. Pamiętaj, że istnieje wiele innych metod, które mogą się przydać w pracy z tekstem, więc zawsze warto korzystać z dokumentacji i eksperymentować z różnymi funkcjami programowania.