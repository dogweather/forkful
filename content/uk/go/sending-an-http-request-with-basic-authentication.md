---
title:                "Go: Надсилання HTTP-запиту з основною аутентифікацією"
simple_title:         "Надсилання HTTP-запиту з основною аутентифікацією"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Чому

Надсилання HTTP-запиту з базовою аутентифікацією є необхідним кроком для доступу до захищеного ресурсу. Це дозволяє перевірити обліковий запис користувача та надати йому доступ до важливої інформації.

## Як

```Go
// Імпортуємо необхідний пакет для роботи з HTTP-запитами
import "net/http"

// Побудуємо запит і встановимо базову аутентифікацію
req, err := http.NewRequest("GET", "https://example.com/api/resource", nil)
req.SetBasicAuth("username", "password")

// Виконаємо запит та перевіримо наявність помилок
resp, err := http.DefaultClient.Do(req)
if err != nil {
    // Обробляємо помилку
}

// Отримаємо тіло відповіді та виведемо його
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    // Обробляємо помилку
}
fmt.Println(string(body))
```

В результаті виконання цього коду, ми отримаємо відповідь з захищеного ресурсу, за допомогою якої можемо продовжити роботу з важливою інформацією.

## Глибокий занурення

Базова аутентифікація використовує заголовок "Authorization" у форматі "Basic username:password", де дані кодуються за допомогою base64. Необхідно бути уважними при передачі цих даних, оскільки вони можуть бути декодовані без особливих зусиль.

## Дивіться також

- [Пакет net/http в Go](https://golang.org/pkg/net/http/)
- [Документація по базовій аутентифікації в HTTP](https://developer.mozilla.org/uk/docs/Web/HTTP/Authentication)