---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Elixir: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що & Чому?
Конвертування рядка в нижній регістр - це процес зміни всіх символів у рядку на великі букви на відповідні малі букви. Програмісти часто використовують цю операцію для того, щоб забезпечити незалежність рядка від регістру, тобто робити його чутливим до регістру.

## Як це зробити:
```Elixir 
string = "Привіт світ!"

IO.puts String.downcase(string) 
```

Вивід:
```
привіт світ!
```

## Глибші дослідження:
Хоча даний функціонал є стандартним у багатьох мовах програмування, в Еліксир це можна зробити за допомогою функції `String.downcase/1`. Існує також альтернативний підхід - використання методу `String.downcase/1` з бібліотеки `String`. Обидва ці варіанти працюють ідентично.

Ще однією важливою деталлю є те, що рядок в Еліксирі - це неямовірно гнучкий тип даних, який підтримує різні методи маніпуляції з ними. Важливо не переплутати функцію перетворення рядка у нижній регістр з функцією зміни регістру першої букви -`String.capitalize/1`.

## Дивіться також:
- Документація Еліксир по функції `String.downcase/1`: https://hexdocs.pm/elixir/String.html#downcase/1
- Стаття "Understanding Different Types of Data in Elixir" на Hackernoon: https://hackernoon.com/understanding-different-types-of-data-in-elixir-ef5531e59ddd