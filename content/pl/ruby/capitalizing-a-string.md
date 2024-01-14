---
title:                "Ruby: Zamiana tekstu na wielkie litery"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek zastanawiałeś się, dlaczego warto umieć kapitalizować ciągi znaków w języku Ruby? Możliwe, że już wiesz, ale jeśli nie, to ten artykuł jest dla Ciebie! Kapitalizowanie ciągów znaków jest przydatną funkcją, która może być wykorzystywana w różnych sytuacjach w kodzie Ruby.

## Jak to zrobić

Aby kapitalizować ciągi znaków w Ruby, możesz użyć metody `capitalize`, która zwraca kopię oryginalnego ciągu z pierwszą literą zamienioną na wielką. Na przykład:
```Ruby
"hello world".capitalize
```
Output:
```
"Hello world"
```
Możesz również użyć metody `upcase`, która zwraca kopię ciągu z wszystkimi literami zamienionymi na duże. Na przykład:
```Ruby
"hello world".upcase
```
Output:
```
"HELLO WORLD"
```
Warto również wspomnieć o metodzie `downcase`, która zwraca kopię ciągu z wszystkimi literami zamienionymi na małe.

## Przekopmy się trochę głębiej

W języku Ruby możesz użyć również notacji `!` po nazwie metody, aby zmodyfikować oryginalny ciąg. Na przykład:
```Ruby
string = "hello world"
string.capitalize!
```
Output:
```
"Hello world"
```
Warto również wiedzieć, że w przypadku metody `capitalize`, tylko pierwsza litera w oryginalnym ciągu zostanie zmieniona na wielką, a reszta zostanie pozostawiona bez zmian. Jeśli chcesz zmienić wszystkie pierwsze litery w każdym wyrazie, możesz użyć metody `titleize`.

## Zobacz również

- Dokumentacja Ruby na temat kapitalizowania ciągów znaków: https://ruby-doc.org/core-2.7.2/String.html#method-i-capitalize
- Przydatne informacje związane z manipulacją tekstem w Ruby: https://www.rubyguides.com/2019/07/ruby-string-methods/
- Przykłady użycia metody `capitalize` w praktyce: https://www.skorks.com/2009/08/better-stringbclass-as-the-key-for-hash-collections-in-ruby/