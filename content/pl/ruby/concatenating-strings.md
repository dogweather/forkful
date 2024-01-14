---
title:    "Ruby: Konkatenacja ciągów znaków"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Dlaczego?

Ciągłe zdobywanie nowych umiejętności jest wciąż ważnym elementem bycia programistą. Jednym z podstawowych elementów programowania jest łączenie lub "konkatenacja" (w odróżnieniu od słowa "konkurencja"!) ciągów znaków. W tym artykule dowiesz się, dlaczego jest to ważne i jak to zrobić w Ruby.

## Jak to zrobić?

Najprostszym sposobem na połączenie ciągów znaków jest użycie operatora plus " + ". Spójrz na poniższy kod:

```ruby
str1 = "Witaj"
str2 = "świecie!"
str3 = str1 + str2
puts str3
```
Output:
```ruby
Witaj świecie!
```
W powyższym kodzie, użyliśmy operatora plus aby połączyć dwa ciągi znaków i przypisaliśmy wynik do nowej zmiennej "str3". Następnie, używając metody "puts", wyświetliliśmy wynik połączenia.

Kolejnym sposobem jest użycie metody "concat", która jest dostępna dla obiektów klasy String w Ruby:

```ruby
str1 = "Witaj"
str2 = "świecie!"
str1.concat(str2)
puts str1
```

Output:
```ruby
Witaj świecie!
```

Oprócz tego, możemy również skorzystać z metody "<<" (append), która dodaje ciągu znaków na końcu istniejącego ciągu:

```ruby
str1 = "Witaj"
str2 = "świecie!"
str1 << str2
puts str1
```

Output:
```ruby
Witaj świecie!
```

Wszystkie te metody pozwalają na szybkie i wygodne łączenie ciągów znaków w Ruby.

## Deep Dive

Konkatenacja jest ważnym elementem programowania, ponieważ pozwala nam na tworzenie bardziej zaawansowanych aplikacji, w których musimy wyświetlać i manipulować dużymi ilościami tekstu. W Ruby, ciągi znaków są elastyczne i pozwalają na zastosowanie wielu różnych metod, takich jak "gsub" czy "split", które ułatwiają pracę z tekstem.

Pamiętaj również, że Ruby oferuje wiele wbudowanych metod do łączenia ciągów, takich jak "concat", "<<" czy "+".

## Zobacz również

Jeśli jesteś początkującym programistą w Ruby, polecamy zapoznanie się z dokumentacją języka, gdzie znajdziesz więcej informacji o konkatenacji i innych przydatnych metodach stringów, takich jak "reverse" czy "chomp". Możesz również przeczytać artykuły na temat podstaw programowania w Ruby na naszej stronie [RubyPolska](https://rubypolska.pl/).