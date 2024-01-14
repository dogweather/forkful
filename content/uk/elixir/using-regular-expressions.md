---
title:                "Elixir: Використання регулярних виразів."
simple_title:         "Використання регулярних виразів."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

Що це: Звичайні регулярні вирази (або regexp) - це потужний інструмент, який дозволяє шукати та заміняти текст у великих обсягах. Вони є необхідним інструментом для будь-якого програміста, який працює з текстовими даними.

Як це зробити: Щоб почати використовувати регулярні вирази в Elixir, вам потрібно використовувати модуль Regex та його функцію match. Ось кілька прикладів, як вони можуть бути використані:

```Elixir
Regex.match?(~r/[a-z]+/, "Hello 123") #=> false
Regex.match?(~r/[a-z]+/, "Hello World") #=> true
Regex.scan(~r/[a-z]+/, "The quick brown fox jumps over the lazy dog") #=> ["The", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"]
```

Поглиблене вивчення: Регулярні вирази мають широкі можливості у відповідності та пошуку тексту. Вони можуть шукати специфічні шаблони, робити заміни та вибирати певні частини тексту для подальшої обробки. Крім того, використання груп з допомогою дужок дозволяє зберігати та повторно використовувати певні частини тексту.

Ознайомтеся з документацією про модуль Regex та дослідіть різні функції та можливості регулярних виразів. Вони допоможуть вам ефективно та продуктивно робити роботу з текстовими даними.

Дивись також: 
- [Elixir School - Regular Expressions](https://elixirschool.com/lessons/basics/regular-expressions/)
- [Офіційна документація Elixir про модуль Regex](https://hexdocs.pm/elixir/Regex.html)
- [Regexp Cheat Sheet](https://medium.com/@frontender_ua/regexp-%D1%88%D0%B5%D0%BB%D1%8C-html-eab5755f7656) (українською)