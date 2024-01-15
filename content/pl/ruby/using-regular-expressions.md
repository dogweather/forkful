---
title:                "Używanie wyrażeń regularnych"
html_title:           "Ruby: Używanie wyrażeń regularnych"
simple_title:         "Używanie wyrażeń regularnych"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Dlaczego

Regularne wyrażenia, znane również jako wyrażenia regularne lub regex, są narzędziem, które pomaga w manipulowaniu i przetwarzaniu tekstów. Dzięki nim można szybko i precyzyjnie znajdować lub zamieniać konkretne fragmenty tekstu. Jest to bardzo przydatne narzędzie dla programistów, którzy potrzebują skutecznego sposobu na analizowanie i edytowanie danych tekstowych.

## Jak używać wyrażeń regularnych w Ruby

Aby używać wyrażeń regularnych w Ruby, musimy użyć dedykowanego operatora `=~` lub metody `match()` dla obiektu napisów. Przykładowe użycie wygląda następująco:

```Ruby
text = "Witaj w świecie Ruby!"
puts "Znaleziono dopasowanie!" if text =~ /Ruby/ 
# output: "Znaleziono dopasowanie!"
```

W powyższym przykładzie, dla zmiennej `text` zostaje wywołany operator `=~` wraz z wyrażeniem regularnym `/Ruby/`, które sprawdza, czy w tekście jest słowo "Ruby". Jeśli tak, zostaje wyświetlony komunikat.

Możemy również wykorzystać metody `scan()` lub `split()` do dalszego przetwarzania tekstu przy użyciu wyrażeń regularnych. Przykładowo:

```Ruby
text = "Mój ulubiony język programowania jest Ruby!"
matches = text.scan(/([A-Z]\w+)/)
# output: [["Mój"], ["Ruby"]] 
```

W tym przypadku, metoda `scan()` wyszukuje wszystkie pasujące fragmenty tekstu i zwraca je w postaci tablicy napisów. Wykorzystano również grupowanie w wyrażeniu regularnym, aby zwrócić tylko wyrazy zaczynające się od dużej litery.

## Głębszy wgląd w użycie wyrażeń regularnych

Ruby oferuje szeroki zakres możliwości przy użyciu wyrażeń regularnych. Możemy między innymi wykorzystać specjalne sekwencje znaków, które ułatwiają dopasowywanie konkretnych wzorców w tekście, wyrażenia regularne wielolinijkowe, które pozwalają na dopasowanie tekstu rozciągniętego na wiele linii oraz zmienne zastępcze, które ułatwiają zamianę fragmentów tekstu w wyniku dopasowania. Istnieją również sposoby na bardziej zaawansowane operacje, takie jak usuwanie duplikatów czy zmiana kolejności słów w tekście przy użyciu wyrażeń regularnych.

## Zobacz także

- [Dokumentacja Ruby o wyrażeniach regularnych](https://ruby-doc.org/core-2.7.1/Regexp.html)
- [Przewodnik po wyrażeniach regularnych w Ruby](https://link.medium.com/qHHlw4MHR8)
- [Kurs "Ruby w 20 minut" z sekcją o wyrażeniach regularnych](https://www.ruby-lang.org/pl/documentation/quickstart/)