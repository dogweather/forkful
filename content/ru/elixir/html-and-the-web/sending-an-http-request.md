---
title:                "Отправка HTTP-запроса"
aliases:
- /ru/elixir/sending-an-http-request.md
date:                  2024-01-29T00:02:26.281237-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/sending-an-http-request.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Отправка HTTP-запроса - это способ, которым ваша программа запрашивает данные из сети, примерно как вы бы попросили библиотекаря дать книгу. Программисты делают это для получения, отправки или манипуляции удаленными данными, от получения погоды до публикации твитов.

## Как это делать:
Используйте библиотеку `HTTPoison` в Elixir. Она аккуратная, простая и делает свою работу.

1. Добавьте HTTPoison в `mix.exs` вашего проекта:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

2. Запустите `mix deps.get` в терминале, чтобы загрузить зависимость.

3. Теперь вы готовы отправить GET-запрос:

```elixir
case HTTPoison.get("https://jsonplaceholder.typicode.com/posts/1") do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.inspect(body) # вы получили свои данные!
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.inspect(reason) # обработка ошибки
end
```

Пример вывода: строка JSON с данными поста из заполнителя API.

## Подробнее
Ранее вы бы использовали `:httpc`, поставляемый с Erlang/OTP или `HTTPotion` от Elixir. Теперь более популярен HTTPoison с более чистым синтаксисом и построенный на базе Hackney - надежного HTTP-клиента для Erlang.

Альтернативы HTTPoison включают Tesla – гибкий HTTP-клиент с поддержкой промежуточного программного обеспечения, и Mint – новый, низкоуровневый HTTP-клиент.

С точки зрения реализации, эти библиотеки обрабатывают управление соединениями, SSL и keep-alive - сложные вещи, которые важны для эффективной отправки HTTP-запросов. Они действуют как дружелюбные библиотекари, которые берут на себя основную работу, чтобы вам не пришлось пробираться через стопки самостоятельно.

## Смотрите также
- [HTTPoison на GitHub](https://github.com/edgurgel/httpoison) – для всех подробностей и обновлений.
- [HexDocs для HTTPoison](https://hexdocs.pm/httpoison) – место для полной документации.
- [Форум Elixir](https://elixirforum.com) – для общения с сообществом.
