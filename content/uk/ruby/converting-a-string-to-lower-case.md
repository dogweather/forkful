---
title:                "Перетворення рядка в нижній регістр"
html_title:           "Javascript: Перетворення рядка в нижній регістр"
simple_title:         "Перетворення рядка в нижній регістр"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?

Перетворення рядків у нижній регістр - це процес зміни усіх великих літер у рядку на малі. Програмісти його використовують для стандартизації рядків, забезпечення однакового трактування різних форм вводу, і для спрощення порівняння і пошуку рядків.

## Як це зробити:

У Ruby перетворити рядок в нижній регістр дуже просто, за допомогою методу `downcase`. Ось простий приклад:

```Ruby
str = "HELLO WORLD"
lower_case_str = str.downcase
puts lower_case_str
```

Виходом цього коду буде `hello world`.

## Поглиблений розбір

В історичному контексті, перетворення рядків в нижній регістр було важливим для старих комп'ютерних систем, які не були сумісними з великими буквами.

Альтернативи до методу `downcase` в Ruby включають `swapcase`, який змінює всі великі літери на малі, а малі — на великі, і `capitalize`, який перетворює першу букву рядка в велику, а всі інші - в малі літери. 

Method `downcase` працює за допомогою подорожі по кожному символу в рядку і зміни великих літер на відповідні малі літери за допомогою кодування таблиці символів.

## Дивіться також:

1. Документація String#downcase в Ruby - [Ruby-Doc](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)
3. Розповіді Stack Overflow про String#downcase в Ruby - [Stack Overflow](https://stackoverflow.com/questions/38375231/ruby-downcase-string)