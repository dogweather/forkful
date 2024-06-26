---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:26.281237-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0434\u0435\u043B\u0430\u0442\
  \u044C: \u0418\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 `HTTPoison` \u0432 Elixir.\
  \ \u041E\u043D\u0430 \u0430\u043A\u043A\u0443\u0440\u0430\u0442\u043D\u0430\u044F\
  , \u043F\u0440\u043E\u0441\u0442\u0430\u044F \u0438 \u0434\u0435\u043B\u0430\u0435\
  \u0442 \u0441\u0432\u043E\u044E \u0440\u0430\u0431\u043E\u0442\u0443. 1. \u0414\u043E\
  \u0431\u0430\u0432\u044C\u0442\u0435 HTTPoison \u0432 `mix.exs` \u0432\u0430\u0448\
  \u0435\u0433\u043E\u2026"
lastmod: '2024-03-13T22:44:44.421705-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 \u0431\
  \u0438\u0431\u043B\u0438\u043E\u0442\u0435\u043A\u0443 `HTTPoison` \u0432 Elixir."
title: "\u041E\u0442\u043F\u0440\u0430\u0432\u043A\u0430 HTTP-\u0437\u0430\u043F\u0440\
  \u043E\u0441\u0430"
weight: 44
---

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
