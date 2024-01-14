---
title:    "Ruby: Konwersja ciągu znaków na małe litery"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Dlaczego

Jedną z podstawowych umiejętności programistycznych jest możliwość manipulacji tekstem. Często musimy zmieniać wielkość liter w tekście, na przykład aby uzyskać spójną formę lub dopasować się do pewnych zasad stylistycznych. W tym artykule dowiesz się, dlaczego warto umieć konwertować ciągi znaków na małe litery i jak to zrobić w języku Ruby.

## Jak to zrobić

Najprostszym sposobem na konwersję tekstu na małe litery jest użycie metody `downcase` na obiekcie typu string. Oto przykładowy kod:

```Ruby
string = "PRZYKŁADOWY TEKST"
puts string.downcase #=> "przykładowy tekst"
```

W powyższym przykładzie, wywołując metodę `downcase` na zmiennej `string`, otrzymujemy jej wartość w postaci małych liter. Metoda ta nie zmienia wartości samej zmiennej, ale zwraca nowy string, zawierający tekst w małych literach.

Możemy także użyć metody `downcase!`, która dokonuje zmiany bezpośrednio na wartości zmiennej. Pamiętaj jednak, żeby używać tej metody z ostrożnością, ponieważ nie można jej cofnąć.

Aby dowiedzieć się więcej o metodzie `downcase` i innych metodach służących do manipulowania tekstem, możesz przeczytać dokumentację języka Ruby [tutaj](https://ruby-doc.org/core-2.6/String.html#method-i-downcase).

## Pogłębiona analiza

Konwersja tekstu na małe litery jest często wykorzystywana przy porównywaniu tekstów w celu uproszczenia i unifikacji ich formy. Jest to także przydatne podczas pracy z bazami danych, gdzie różnice w wielkości liter mogą powodować błędy.

Warto zauważyć, że metoda `downcase` nie działa jedynie na literach alfabetu angielskiego, ale na wszystkich znakach Unicode, włączając w to polskie znaki, cyfry i znaki specjalne.

## Zobacz także

- Więcej informacji o metodzie `downcase` w języku Ruby znajdziesz [tutaj](https://ruby-doc.org/core-2.6/String.html#method-i-downcase).
- Jeśli interesuje Cię temat manipulacji tekstem, przeczytaj również o metodach `upcase` i `capitalize` [tutaj](https://ruby-doc.org/core-2.6/String.html#method-i-upcase) i [tutaj](https://ruby-doc.org/core-2.6/String.html#method-i-capitalize).
- Warto także zwrócić uwagę na różnice między metodami `downcase` i `capitalize` w [tym](https://www.techiedelight.com/perform-case-insensitive-comparison-two-strings-ruby/) artykule.