---
title:                "Ruby: Wyszukiwanie i zamiana tekstu"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Dlaczego

Czy kiedykolwiek znalazłeś się w sytuacji, w której musiałeś zmienić określone wyrażenia w długim tekście? Na przykład, gdy chcesz zmienić swoje imię we wszystkich wiadomościach e-mail lub gdy musisz dostosować plik dziennika do nowych wymagań? W takich sytuacjach znajomość techniki wyszukiwania i zamieniania tekstu może okazać się niezwykle przydatna. W tym artykule dowiecie się, jak łatwo i szybko można wykonać te czynności w języku Ruby.

## Jak to zrobić

W Ruby istnieje kilka metod, które umożliwiają nam wyszukiwanie i zamienianie tekstu w sposób precyzyjny i efektywny. Pierwszą z nich jest metoda `gsub` (global substitution), która pozwala na zmianę wszystkich wystąpień danego wyrażenia w tekście. Przykładowo, jeśli chcemy zmienić wszystkie cyfry w tekście na litery, możemy użyć tej metody w następujący sposób:

```Ruby
text = "123 Hello World 789"
new_text = text.gsub(/\d+/, 'abc')
puts new_text
# Output: "abc Hello World abc"
```

Możemy również użyć metody `sub` (substitution), która zmienia tylko pierwsze wystąpienie danego wyrażenia. Przykładowo, gdy chcemy zmienić pierwsze wystąpienie słowa "hello" na "cześć", możemy użyć tej metody w ten sposób:

```Ruby
text = "Hello World, hello Earth"
new_text = text.sub("hello", "cześć")
puts new_text
# Output: "Hello World, cześć Earth"
```

## Głębsze zanurzenie

Aby dokładniej zrozumieć działanie metod `gsub` i `sub`, warto poznać podstawy wyrażeń regularnych (regular expressions), które są wykorzystywane w tych funkcjach. Służą one do określania wzorców, które będą wyszukiwane i zamieniane w tekście. Na przykład, wyrażenie `/\d+/` oznacza wszystkie wystąpienia jednej lub więcej cyfr w tekście.

Ponadto, metoda `gsub` może przyjmować jako argument blok kodu, co pozwala na bardziej zaawansowane operacje na tekście. Przykładowo, gdy chcemy zamienić wszystkie słowa na ich odwrócone wersje, możemy użyć bloku kodu w następujący sposób:

```Ruby
text = "hello world"
new_text = text.gsub(/\w+/) { |word| word.reverse }
puts new_text
# Output: "olleh dlrow"
```

## Zobacz również

- [Dokumentacja metody `gsub` w języku Ruby](https://ruby-doc.org/core-3.0.1/String.html#method-i-gsub)
- [Kurs "Regular Expressions in Ruby" na Codecademy](https://www.codecademy.com/learn/learn-regular-expressions)
- [Blog RubyGuides - "Learn to Love Regular Expressions in Ruby"](https://www.rubyguides.com/ruby-tutorial/ruby-regular-expressions/)