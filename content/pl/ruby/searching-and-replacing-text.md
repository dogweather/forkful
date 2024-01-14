---
title:    "Ruby: Wyszukiwanie i zastępowanie tekstu"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego

Wiele razy, przy pisaniu kodu w języku Ruby, mogą zdarzyć się sytuacje, w których musimy dokonać zmiany w tekście. Może to być konieczne do naprawienia błędów, aktualizacji danych lub po prostu aby dostosować tekst do naszych potrzeb. Dlatego ważne jest, aby znać możliwości wyszukiwania i zamiany tekstu w języku Ruby.

## Jak to zrobić

W języku Ruby istnieje kilka sposobów na wyszukiwanie i zamianę tekstu. Jedną z najczęściej wykorzystywanych metod jest użycie metody `gsub`. Zwróć uwagę, że ta metoda może zamieniać kilka wystąpień danego wyrażenia jednocześnie. Przykładowy kod wyglądałby następująco:

```Ruby
text = "Dzień dobry, świecie!"
changed_text = text.gsub("świecie", "universum")
puts changed_text
```

Output: Dzień dobry, universum!

Możemy także wykorzystać wyrażenia regularne do wyszukiwania i zamiany tekstu. Jest to szczególnie przydatne, jeśli chcemy dokonać bardziej skomplikowanych zmian. Przykładowy kod wyglądałby następująco:

```Ruby
text = "Dzień dobry, ŚwieTecznY!"
changed_text = text.gsub(/[A-Z]/, "*")
puts changed_text
```

Output: *zień *obry, *wie*ecn*!

## Głębszy zanurzenie

Metoda `gsub` działa na klasie String, więc możemy jej użyć bezpośrednio na zmiennych typu string. Istnieją jednak także metody `gsub!` oraz `sub`, które działają na tym samym zasadzie, ale modyfikują oryginalny ciąg znaków, bez tworzenia nowego obiektu. Możemy także określić liczbę wystąpień, które chcemy zamienić lub użyć bloku do dostosowania zamiany w zależności od warunków.

## Zobacz także

* Dokumentacja Ruby o metodzie `gsub`:https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub
* Wprowadzenie do wyrażeń regularnych w Ruby: https://www.rubyguides.com/ruby-tutorial/regex/