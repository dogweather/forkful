---
title:    "Ruby: Konwertowanie daty na ciąg znaków"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Dlaczego

Jednym z ważnych zadań programisty jest umiejętność manipulowania danymi w różnych formatach. Często stosowanym formatem jest data, jednak zdarza się, że musimy ją przekonwertować na tekst, aby móc wykorzystać ją w naszej aplikacji. W tym artykule odkryjemy jak w prosty sposób wykonać tę konwersję w języku Ruby.

## Jak to zrobić

W Ruby mamy dostęp do metody `to_s`, która pozwala na przekonwertowanie daty na string. Przykładowo, jeśli chcemy wyświetlić aktualną datę w formacie DD.MM.YYYY należy wywołać `Time.now.to_s('%d.%m.%Y')`. Oto pełny kod i jego wynik:

```Ruby
puts Time.now.to_s('%d.%m.%Y')

# 14.08.2021
```

Możemy również wykorzystać metodę `strftime` aby określić własny format daty. Poniższy przykład wyświetli datę w formacie Dzień Tygodnia, DD.MM.YYYY.

```Ruby
puts Time.now.strftime('%A, %d.%m.%Y')

# Sobota, 14.08.2021
```

## Głębsze zanurzenie

W Ruby istnieje wiele metod i opcji, które pozwalają na dokładną kontrolę nad konwersją daty na string. Warto zapoznać się z dokumentacją, aby poznać wszystkie możliwości. Najważniejsze rzeczy, które należy pamiętać, to użycie wielkich liter w formatowaniu daty (np. `%d` zwróci dzień jako liczby, `%D` jako datę w formacie MM/DD/RRRR) oraz sprawdzenie różnic w formacie dat w zależności od systemu operacyjnego.

## Zobacz również

- [Dokumentacja Ruby: Time](https://ruby-doc.org/core-2.5.1/Time.html)
- [Pełna lista formatowanie daty w Ruby](http://ruby-doc.org/core-2.2.0/Time.html#method-i-strftime)
- [Poradnik dla początkujących: Manipulacja datami w Ruby](https://www.rubyguides.com/2019/02/ruby-time/)