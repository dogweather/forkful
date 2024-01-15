---
title:                "Надсилання запиту http з базовою автентифікацією"
html_title:           "Elixir: Надсилання запиту http з базовою автентифікацією"
simple_title:         "Надсилання запиту http з базовою автентифікацією"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Базова автентифікація є одним з методів автентифікації у мережевому протоколі HTTP. Вона дозволяє вхід на захищені сторінки з використанням ідентифікатора та паролю. Використання базової автентифікації є важливим для безпечних веб-додатків та API.

## Як

```elixir
# Налаштування HTTP-клієнта з базовими автентифікаційними даними
client = HTTPoison.BasicAuth.client("john", "password")

# Відправлення GET-запиту на захищений ресурс
HTTPoison.get("https://example.com/protected_resource", [], client)

# Виведення коду статусу та тіла відповіді
status = %{code: 200}
body = "Hello, world!"
```

## Глибоке вивчення

При відправленні HTTP-запиту з базовою автентифікацією, ідентифікатор та пароль посилаються в запиті у формі "ім'я:пароль", які потім кодуються у форматі Base64 та додаються до заголовка "Authorization". При отриманні запиту, сервер декодує ці дані та перевіряє їх правильність для доступу до захищеного ресурсу.

## Дивись Також

- [Elixir](https://elixir-lang.org/) - Офіційний сайт мови Elixir.
- [HTTPoison](https://hexdocs.pm/httpoison/HTTPoison.html) - Розширення для взаємодії з веб-серверами та API.
- [Basic Authentication](https://developers.google.com/web/updates/2016/03/http-security-reporting-for-developers#basic_auth) - Документація Google про базову автентифікацію у веб-розробці.