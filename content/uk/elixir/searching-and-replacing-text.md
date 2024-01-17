---
title:                "Пошук та заміна тексту"
html_title:           "Elixir: Пошук та заміна тексту"
simple_title:         "Пошук та заміна тексту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

Co потрiбно зробити, щоб замiнити текст в Elixir

## Що & Навiщо?

Замiна тексту - це процес замiни одного шматка тексту на iнший у програмному кодi. Програмiсти часто роблять це для виправлення помилок, оновлення бiблiотек або змiни функцiоналу своїх програм.

## Як:

```Elixir
# Замінити слово "програмування" на "розробки" у строці
text = "Я люблю програмування"
text = String.replace(text, "програмування", "розробки")
IO.puts(text)

# Виведе "Я люблю розробки"
```

```Elixir
# Замінити всі голоснi лiтери у строці на "x"
text = "Це речення містить голосні літери"
text = String.replace(text, ~r/[aeiouyіїуеєаою]/, "x")
IO.puts(text)

# Виведе "Cx рxченнx mxстить gxлxsнx лxтxрx"
```

## Глибоке погруження:

Заміна тексту - це поширений процes у програмуваннi, що датується з часiв появи перших мов програмування. Є багато альтернативних способів заміни тексту, таких як застосування регулярних виразів чи використання бібліотек для обробки тексту. У Elixir для заміни тексту використовується функцiя `String.replace/3` з вбудованої бібліотеки `String`, що дозволяє замінювати не тільки текст, але й регулярні вирази та шаблони.

## Дивіться також:

- [Офіційна документація Elixir String.replace/3](https://hexdocs.pm/elixir/String.html#replace/3)
- [Туторіал про використання регулярних виразів у Elixir](https://elixirschool.com/lessons/basics/regex/)
- [Додаткові бібліотеки для обробки тексту у Elixir](https://github.com/h4cc/awesome-elixir#text-and-strings)