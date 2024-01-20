---
title:                "Завантаження веб-сторінки"
html_title:           "Gleam: Завантаження веб-сторінки"
simple_title:         "Завантаження веб-сторінки"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Що і Чому?
Скачування веб-сторінки - це процес зчитування інформації з URL адреси. Програмісти роблять це, щоб аналізувати вміст сторінки, автоматизувати робочі процеси чи створити бекапи сторінок.

## Як зробити:
Для скачування веб-сторінок в Elixir нам знадобиться HTTP бібліотека, наприклад, HTTPoison. Установка та використання HTTPoison відображено нижче:

```Elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

Після додавання залежностей, запустити команду `mix deps.get` в ваших терміналах. Потім створимо запит:

```Elixir
defmodule MyApp.WebScraper do
  require HTTPoison

  def fetch_page(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Received status code #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end
```

## Пірнання в деталі:
Загрузка веб-сторінок - це старий як інтернет концепт, потреба в якому постійно зростає. Спочатку програмісти створювали власні скрипти для скачування сторінок, але з розвитком мови програмування, були створені спеціальні бібліотеки, такие як HTTPoison в Elixir.

Як альтернатива, можливо використовувати інші бібліотеки, такі як Hackney чи :httpc, яка є частиною OTP. Обидва варіанти достатньо гнучкі, але HTTPoison надає більш вдалий, що до функцій interfeys.

Щодо деталей реалізації, HTTPoison використовує бібліотеку Hackney в якості транспорту HTTP і надає більш "Elixir-friendly" API для виконання HTTP запитів.

## Див. також:
1. [HTTPoison Docs](https://hexdocs.pm/httpoison/readme.html)
2. [Hackney GitHub](https://github.com/benoitc/hackney)
3. [Elixir :httpc](https://erlang.org/doc/man/httpc.html)