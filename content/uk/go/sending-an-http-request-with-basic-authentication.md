---
title:                "Надсилання HTTP-запиту з базовою автентифікацією"
date:                  2024-01-20T18:01:47.290024-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту з базовою автентифікацією"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?
HTTP-запит з базовою аутентифікацією — це спосіб передавання логіна і пароля серверу для перевірки прав доступу. Програмісти роблять це для безпечної взаємодії з API, які вимагають авторизації.

## Як це зробити:
```Go
package main

import (
    "encoding/base64"
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    client := &http.Client{}
    req, err := http.NewRequest("GET", "http://example.com/data", nil)
    if err != nil {
        panic(err)
    }

    username := "user"
    password := "pass"
    req.Header.Set("Authorization", "Basic " + base64.StdEncoding.EncodeToString([]byte(username+":"+password)))

    resp, err := client.Do(req)
    if err != nil {
        panic(err)
    }
    defer resp.Body.Close()
    
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        panic(err)
    }
    fmt.Println(string(body))
}
```
У коді вище створюється HTTP-запит із заголовком `Authorization`, що містить облікові дані у форматі Base64.

## Поглиблений розбір:
Базова HTTP-аутентифікація з'явилася на самому початку існування HTTP протоколу і іноді критикується за відсутність шифрування облікових даних. Альтернативи, такі як OAuth, набагато безпечніші в цьому плані. Те, що базова аутентифікація передає логін та пароль у нешифрованому вигляді (лише кодування Base64), робить її вразливою до перехоплення третім особами, особливо через незахищені з'єднання.

Використовуйте HTTPS для захисту даних під час транзиту. Go стандартна бібліотека http підтримує методи автоматичного додавання заголовків авторизації без необхідності ручного кодування Base64.

## Також рекомендується:
- [RFC 7617, 'The 'Basic' HTTP Authentication Scheme'](https://datatracker.ietf.org/doc/html/rfc7617)
- [Go by Example: HTTP Clients](https://gobyexample.com/http-clients)
- [Secure Your Data with HTTP Headers](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers)
- [OAuth 2.0](https://oauth.net/2/)