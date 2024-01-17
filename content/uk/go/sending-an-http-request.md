---
title:                "Відправка http запиту."
html_title:           "Go: Відправка http запиту."
simple_title:         "Відправка http запиту."
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що і чому?

Відправлення HTTP-запиту - це коли в програмі ви надсилаєте запит до веб-сервера. Програмісти це роблять, щоб отримати доступ до даних або виконати деякі дії на сервері.

## Як це зробити:

Існує кілька способів відправки HTTP-запиту в Go. Один з них використовує пакет "net/http", який ми і розглянемо. Приклад:

```
package main

import (
  "fmt"
  "net/http"
)

func main() {
  resp, err := http.Get("https://example.com")
  if err != nil {
    panic(err)
  }
  defer resp.Body.Close()

  fmt.Println("Статусний код:", resp.Status)
}
```

Вивід: Статусний код: 200 OK

## Занурення в глибину:

HTTP - це протокол передачі даних в інтернеті, і він існує з 1991 року. У Go є кілька пакетів, які можуть допомогти вам відправити HTTP-запит, такі як "net/http" для базових запитів і "net/http/httputil" для розширених функцій, які допомагають побудувати запити. Існують і інші альтернативи, такі як пакет "netcurl", який дозволяє вам використовувати бібліотеку CURL для відправлення запитів.

## Дивіться також:

https://golang.org/pkg/net/http/ - офіційна документація Go для пакету "net/http" з прикладами використання.