---
title:                "Отправка HTTP-запроса с базовой аутентификацией"
date:                  2024-01-29T00:02:50.558583-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отправка HTTP-запроса с базовой аутентификацией"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/gleam/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-29, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Отправка HTTP-запроса с базовой аутентификацией означает, что вы добавляете имя пользователя и пароль к вашему запросу, как секретное рукопожатие для доступа к веб-ресурсу. Программисты делают это, чтобы убедиться, что доступ получают только посвященные (т.е., те, кто обладает правильными учетными данными).

## Как это сделать:

В Gleam используйте библиотеку `gleam/http`. Вот упрощенный пример:

```gleam
import gleam/http.{Request, BasicAuth, get}

pub fn fetch_resource_with_basic_auth() {
  let auth = BasicAuth(
    username: "awesome_dev",
    password: "superSecret123",
  )
  let request = Request(
    method: Get,
    url: "https://api.example.com/protected",
    headers: [],
    body: None,
    basic_auth: Some(auth),
  )

  assert Ok(response) = get(request)
  response
}
```

Что вы увидите при запуске:

```gleam
Ok(#{
  status: 200,
  headers: [...],
  body: "Вот ваши секретные данные!"
})
```

Или, если учетные данные неверны:

```gleam
Ok(#{
  status: 401,
  headers: [...],
  body: ""
})
```

## Глубокое погружение

В старые времена базовая аутентификация была одним из первых методов защиты веб-сообщений; это как старый висячий замок — простой, но не самый безопасный.

Альтернативы? У вас есть OAuth для более сложных сценариев, или аутентификация на основе токенов в качестве компромисса между простотой и безопасностью.

С точки зрения реализации, базовая аутентификация в HTTP вставляет Base64 закодированную строку (вашу комбинацию имени пользователя и пароля) в заголовок 'Authorization'. Это не зашифровано, поэтому это базовая аутентификация и не рекомендуется для чувствительных данных без использования HTTPS как минимум. Также не жестко кодируйте учетные данные в вашем коде; используйте переменные среды или безопасный сервис хранилища.

## Смотрите также

Углубитесь в:

- [Аутентификация в HTTP на MDN](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Безопасное хранение секретов приложения](https://learn.microsoft.com/ru-ru/aspnet/core/security/app-secrets)
