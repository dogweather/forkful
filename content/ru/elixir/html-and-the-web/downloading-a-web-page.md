---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:43.726012-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Elixir, \u0431\u043B\u0430\u0433\u043E\u0434\u0430\u0440\u044F \u0441\
  \u0432\u043E\u0438\u043C \u043C\u043E\u0449\u043D\u044B\u043C \u0431\u0438\u0431\
  \u043B\u0438\u043E\u0442\u0435\u043A\u0430\u043C HTTP-\u043A\u043B\u0438\u0435\u043D\
  \u0442\u0430, \u0434\u0435\u043B\u0430\u0435\u0442 \u044D\u0442\u0443 \u0437\u0430\
  \u0434\u0430\u0447\u0443 \u043B\u0435\u0433\u043A\u043E\u0439. \u0412\u043E\u0442\
  \ \u043A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\u0442\u044C\
  \ \u0441 `HTTPoison`."
lastmod: '2024-03-13T22:44:44.425169-06:00'
model: gpt-4-0125-preview
summary: "Elixir, \u0431\u043B\u0430\u0433\u043E\u0434\u0430\u0440\u044F \u0441\u0432\
  \u043E\u0438\u043C \u043C\u043E\u0449\u043D\u044B\u043C \u0431\u0438\u0431\u043B\
  \u0438\u043E\u0442\u0435\u043A\u0430\u043C HTTP-\u043A\u043B\u0438\u0435\u043D\u0442\
  \u0430, \u0434\u0435\u043B\u0430\u0435\u0442 \u044D\u0442\u0443 \u0437\u0430\u0434\
  \u0430\u0447\u0443 \u043B\u0435\u0433\u043A\u043E\u0439."
title: "\u0417\u0430\u0433\u0440\u0443\u0437\u043A\u0430 \u0432\u0435\u0431-\u0441\
  \u0442\u0440\u0430\u043D\u0438\u0446\u044B"
weight: 42
---

## Как это сделать:
Elixir, благодаря своим мощным библиотекам HTTP-клиента, делает эту задачу легкой. Вот как это сделать с `HTTPoison`:

```elixir
# Сначала добавьте HTTPoison в зависимости вашего mix.exs:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Выполните mix deps.get, чтобы загрузить новую зависимость

# Теперь давайте загрузим веб-страницу:
defmodule PageDownloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Получен код состояния: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end

# Пример использования:
{:ok, contents} = PageDownloader.download("http://example.com")
```

Не забывайте обрабатывать потенциальные ошибки, чтобы избежать сбоев!

## Подробный обзор
Подход Elixir к веб-взаимодействиям поддерживается надёжными сетевыми возможностями Erlang. `HTTPoison` — популярная библиотека, построенная на основе `hackney`, но это не единственный игрок. Есть также `Tesla`, которая предлагает более модульный подход с поддержкой промежуточного программного обеспечения (middleware).

Исторически загрузка веб-контента была более ручной, включая создание HTTP-запросов через сокеты. Библиотеки Elixir абстрагируют эти детали, позволяя вам сосредоточиться на логике вашего приложения.

При загрузке веб-страниц вы сталкиваетесь с асинхронными операциями и различными HTTP-протоколами, которые Elixir обрабатывает с грацией благодаря своей модели параллелизма и отказоустойчивому дизайну. Кроме того, критически важна обработка текстовых и двоичных данных — убедитесь, что вы учитываете кодировку и потенциал двоичных данных в веб-контенте.

## Смотрите также
- [Документация `HTTPoison`](https://hexdocs.pm/httpoison)
- [Библиотека `Tesla` на Hex](https://hex.pm/packages/tesla)
- [Руководство по OTP Concurrency от Elixir School](https://elixirschool.com/en/lessons/advanced/otp_concurrency/)
- [Библиотека `hackney` от Erlang](https://github.com/benoitc/hackney)
