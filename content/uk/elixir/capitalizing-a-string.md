---
title:                "Перетворення рядка на великі літери"
html_title:           "Arduino: Перетворення рядка на великі літери"
simple_title:         "Перетворення рядка на великі літери"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?
Капіталізація рядків — процес перетворення першої літери слова у верхній регістр. Це корисно для відформатування тексту, наприклад, при підготовці заголовків або імен.

## Як робити:
```elixir
# Основний спосіб капіталізації рядка
defmodule MyString do
  def capitalize(string) do
    string
    |> String.trim()
    |> String.capitalize()
  end
end

IO.puts MyString.capitalize("еліксир")  # "Еліксир"
```

```elixir
# Капіталізація всіх слів у реченні
defmodule MyString do
  def capitalize_each_word(sentence) do
    sentence
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

IO.puts MyString.capitalize_each_word("вітаю в світі еліксир!")  # "Вітаю В Світі Еліксир!"
```

## Поглиблений аналіз
Еліксир, як і більшість мов програмування, надає вбудовані інструменти для роботи з текстовими рядками. Функція `String.capitalize/1` є частиною стандартної бібліотеки і з'явилась у ранніх версіях мови.

Капіталізація рядків може відрізнятися залежно від локалізації, але, на жаль, `String.capitalize/1` працює однаково для всіх мов і може не враховувати особливості української мови. Тому вам може бути потрібно писати власне рішення для капіталізації, яке краще враховує локалізацію.

Також є альтернативні способи капіталізації, наприклад, при використанні регулярних виразів або інших мов програмування, де можуть бути різні функції для цього. У кожній мові своя ідіоматика і оптимальні шляхи досягнення результату.

## Дивіться також
- [Elixir String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Unicode Standard Annex #29](https://unicode.org/reports/tr29/) – для розуміння, як правильно працювати з символами Unicode
- [Elixir School — Strings](https://elixirschool.com/en/lessons/basics/strings) – більше про рядки в Еліксир