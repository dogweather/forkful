---
title:                "Надсилаємо HTTP-запит з базової аутентифікацією"
html_title:           "C#: Надсилаємо HTTP-запит з базової аутентифікацією"
simple_title:         "Надсилаємо HTTP-запит з базової аутентифікацією"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що і чому?
При відправленні HTTP-запиту з основною аутентифікацією, ми надаємо перевірені дані (логін та пароль) для доступу до ресурсів. Це зазвичай використовують програмісти, щоб обмежувати доступ до веб-служб та серверів.

## Як це зробити:
Виконаємо простий запит GET з базовою аутентифікацією. Для цього використовуємо стандартну бібліотеку net/http.

```Go
package main

import (
	"fmt"
	"net/http"
)

func main() {
	// Створюємо клієнт HTTP
	client := &http.Client{}

	// Генеруємо запит
	req, err := http.NewRequest("GET", "http://example.com", nil)
	if err != nil {
		log.Fatalln(err)
	}

	// Встановлюємо аутентифікацію
	req.SetBasicAuth("username", "password")

	// Відправляємо запит
	resp, err := client.Do(req)
	if err != nil {
		log.Fatalln(err)
	}

	// Виводимо код статусу
	fmt.Println("Status code:", resp.StatusCode)
}
```

## Поглиблений занурення
У минулому, базова аутентифікація вважалась цілком прийнятною, але тепер вона більше не вважається достатньою з точки зору безпеки, особливо при використанні через незахищені протоколи.
Однією з альтернатив є аутентифікація Bearer, яка часто використовується разом з OAuth2. У реалізації на Go немає нічого особливого, ви просто встановлюєте заголовок "Authorization" зі значенням "Basic " + base64("username:password").

## Дивіться також:
1. [Go HTTP client example](https://golang.org/pkg/net/http/#Client)
2. [Basic Access Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
3. [Understanding HTTP Basic Authentication](https://betterprogramming.pub/understanding-http-authentication-schemes-bearer-vs-basic-c2567dd4588d)