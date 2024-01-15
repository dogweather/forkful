---
title:                "Надсилання запиту http"
html_title:           "Go: Надсилання запиту http"
simple_title:         "Надсилання запиту http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Чому
Програми, що використовують інтернет, часто повинні взаємодіяти з іншими серверами для отримання необхідних даних. Найпоширенішим способом цього здійснення є відправка HTTP запиту, щоб отримати відповідь від сервера. У цій статті ми поговоримо про те, як відправляти HTTP запити за допомогою мови програмування Go.

## Як
Для відправки HTTP запиту використовують функцію `http.Get()`. Спочатку потрібно імпортувати пакет `net/http`:

```Go
import "net/http"
```

Потім можна створити HTTP GET запит до URL-адреси і отримати відповідь від сервера:

```Go
resp, err := http.Get("https://example.com")
```

Перша змінна `resp` буде містити об'єкт відповіді, а друга `err` - буде містити помилку (якщо вона виникла). Можна перевірити, чи виникла помилка, за допомогою наступного умовного оператора:

```Go
if err != nil {
  // обробка помилки
} else {
  // обробка відповіді
}
```

Для отримання даних з відповіді можна використовувати метод `resp.Body.Read()`, наприклад:

```Go
respBody := make([]byte, resp.ContentLength)
resp.Body.Read(respBody)
// далі можна обробити respBody
```

## Deep Dive
Існує багато параметрів, які можна налаштувати для HTTP запиту в Go, наприклад, заголовки (headers) або параметри запиту (query parameters). Для цього можна скористатись структурою `http.Request`:

```Go
req, err := http.NewRequest("GET", "https://example.com", nil)
```

Тут першим параметром є метод запиту (у нашому випадку, GET), другим - URL-адреса, а третім - тіло запиту. Детальніше про цю структуру можна дізнатися в [документації Go](https://golang.org/pkg/net/http/#Request).

## See Also
- [Пакет `net/http` в документації Go](https://golang.org/pkg/net/http/)
- [Розділ про HTTP в документації Go](https://golang.org/doc/articles/wiki/#tmp_4)