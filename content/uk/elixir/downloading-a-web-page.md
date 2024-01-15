---
title:                "Завантаження веб-сторінки"
html_title:           "Elixir: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Чому

Завантаження веб-сторінки є необхідною задачею для багатьох проектів, таких як веб-скрапінг, автоматизація тестування та отримання даних для аналізу. Використовуючи Elixir, ви можете швидко і ефективно завантажити веб-сторінку та отримати необхідні дані.

## Як 

Користувачі Elixir можуть завантажувати веб-сторінки за допомогою пакету `HTTPoison`. Перш за все, необхідно встановити цей пакет, використовуючи команду `mix deps.get`. Потім можна використати функцію `HTTPoison.get` для здійснення запиту до веб-сторінки. Нижче наведено приклад коду та вихідних даних для завантаження веб-сторінки Google.

```Elixir
response = HTTPoison.get("https://www.google.com")
IO.puts(response.body)
```

```HTML
<!doctype html>
<html>
<head>
...
</head>
<body>
...
</body>
</html>
```

## Глибшого дослідження

Функція `HTTPoison.get` повертає структуру даних `HTTPoison.Response`, яку можна перетворити на HTML за допомогою бібліотеки `Floki`. Крім того, можна налаштувати запит, вказавши параметри, такі як заголовки або дані за допомогою методу `HTTPoison.request`. Також, можна використовувати паралельні запити за допомогою пакета `Mint`. Для детальнішої інформації, перегляньте документацію по пакетах `HTTPoison`, `Floki` та `Mint`.

## Дивись також

- [Офіційна документація Elixir](https://hexdocs.pm/elixir/Kernel.html)
- [Документація по пакету HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Документація по пакету Floki](https://hexdocs.pm/floki/Floki.html)
- [Документація по пакету Mint](https://hexdocs.pm/mint/Mint.HTTP.html)