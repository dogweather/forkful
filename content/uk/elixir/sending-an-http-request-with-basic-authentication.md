---
title:                "Відправка запиту http з базовою автентифікацією"
html_title:           "Elixir: Відправка запиту http з базовою автентифікацією"
simple_title:         "Відправка запиту http з базовою автентифікацією"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

"## Що і чому?"

Ви навряд чутіли про термін "відправка HTTP-запиту з базовою аутентифікацією", але для програмістів це один з поширених способів працювати з веб-сервісами. Він дозволяє надійно передавати ідентифікатори та паролі для авторизації на сервері, щоб отримати доступ до захищених даних.

"## Як це зробити:"

```Elixir
defmodule HTTPRequest do
  def basic_auth_request(url, username, password) do
    # Створюємо зазначений URL з урахуванням ідентифікатора та пароля
    url_with_credentials = url
      |> URI.join()
      |> URI.add_query_param("user", username)
      |> URI.add_query_param("password", password)

    # Відправляємо GET-запит з базовою аутентифікацією
    response = HTTPoison.get(url_with_credentials, [], [basic_auth: {username, password}])

    # Перевіряємо статус відповіді та повертаємо вміст відповіді
    case response do
      {:ok, %{status_code: 200, body: body}} -> body
      _ -> []
    end
  end
end
```

Приклад використання:

```Elixir
username = "user123"
password = "secret"

# Відправляємо запит до стороннього API з базовою аутентифікацією і отримуємо відповідь
url = "https://example.com/api"
response = HTTPRequest.basic_auth_request(url, username, password)

IO.puts response
# {"message": "Дякуємо за запит!"}
```

"## Глибока занурення:"

Відправка HTTP-запиту з базовою аутентифікацією була стандартизована у 1999 році з метою підвищити рівень безпеки веб-серверів та зменшити кількість шахраїв, які можуть отримати доступ до захищених даних.

Є інші способи авторизації на сервері, такі як OAuth або власні методи, проте базова аутентифікація є простим та ефективним рішенням у багатьох випадках. Вона також підтримується більшістю веб-серверів.

Реалізація надійної базової аутентифікації включає кодування ідентифікатора та пароля у форматі Base64. Для даної мети можна використати функції Elixir такі як `Base.encode64/1` та `Base.decode64/1`.

"## Дивіться також:"

- Документація Elixir по відправленню HTTP-запитів з HTTPoison: https://hexdocs.pm/httpoison/HTTPoison.html
- Розділ RFC про базову аутентифікацію: https://tools.ietf.org/html/rfc2617