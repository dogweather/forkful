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

## Co i dlaczego?

Używanie wyrażeń regularnych, czyli krótkich i zwięzłych wzorców znaków, jest niezbędnym narzędziem dla programistów Ruby. Dzięki nim możemy szybko i precyzyjnie znajdować i manipulować tekstem w programach. Innymi słowy, jest to potężne narzędzie, które pomaga nam przetwarzać dane w wysoce efektywny sposób.

## Jak to zrobić?

```Ruby
# Szukanie wystąpienia słowa "programowanie" w tekście
tekst = "Uwielbiam programowanie w Ruby!"
if (tekst =~ /programowanie/)
  puts "Znalazłem!"
end
```

```Ruby
# Zamiana wszystkich liter na wielkie
tekst = "to jest super fajny program"
zamieniony_tekst = tekst.upcase
puts zamieniony_tekst
# Output: TO JEST SUPER FAJNY PROGRAM
```

## W głąb informacji

Wyrażenia regularne mają swoje korzenie w matematyce, a zostały wprowadzone do programowania w latach 50. XX wieku. W Ruby, możemy również używać metody `match` zamiast operatora `=~`, ale często jest to kwestia preferencji. Alternatywnym rozwiązaniem dla wyrażeń regularnych są parser-y tekstowe, ale często są one bardziej skomplikowane w użyciu.

## Zobacz też

- [Dokumentacja Ruby o wyrażeniach regularnych](https://ruby-doc.org/core-3.0.0/Regexp.html)
- [Artykuł na temat wyrażeń regularnych w Ruby](https://www.freecodecamp.org/news/learn-regular-expressions-in-about-55-minutes/)
- [Podstawy wyrażeń regularnych w Ruby](https://www.rubyguides.com/ruby-tutorial/regex/)