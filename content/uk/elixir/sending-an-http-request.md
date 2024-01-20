---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Надсилання HTTP-запиту - це процес взаємодії з сервером за допомогою HTTP-протоколу. Програмісти роблять це, щоб отримати, відправити або оновити дані на сервері. 

## Як це робити:

У Elixir ви можете використовувати бібліотеку `HTTPoison`. Встановіть її, додавши в `mix.exs`:

``` Elixir
defp deps do
  [{:httpoison, "~> 1.8"}]
end
```

Потім ви можете надіслати HTTP-запит так:

``` Elixir
{:ok, response} = HTTPoison.get("http://example.com")
IO.inspect(response.status_code) # наприклад, 200
IO.inspect(response.body) # Вміст сторінки
```

І ось ви отримали відповідь від сервера!

## Поглиблений матеріал

1. **Історичний контекст**. Взаємодія за допомогою HTTP запитів виникла з появою WWW у 1991 році. Elixir, яким ми користуємося сьогодні, був створений в 2011 році і з того часу активно використовуєтсья для створення розподілених, відмовостійких систем.

2. **Альтернативи**. В Elixir є інші бібліотеки для роботи з HTTP-запитами, наприклад, `Tesla`. Вибір залежить від ваших потреб та вимог.

3. **Деталі впровадження**. Коли ви використовуєте HTTPoison, Elixir відправляє запрос до серверу, а потім очікує відповіді. Після отримання відповіді, Elixir візьме відповідь та поверне її до вашого коду.

## Дивіться також

1. [HTTPoison документація](https://hexdocs.pm/httpoison/HTTPoison.html)
2. [Еліксир документація](https://elixir-lang.org/docs.html)
3. [HTTP запити: вступ для новачків ](https://uk.wikipedia.org/wiki/HTTP)
   
Якщо ви хочете глибше зануритися в тему, відвідайте посилання, зазначені вище. Успіхів у вивченні Elixir!