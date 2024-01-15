---
title:                "Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem"
html_title:           "Javascript: Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wykorzystanie uwierzytelniania podstawowego jest ważnym elementem bezpieczeństwa przy wysyłaniu żądań HTTP. Dzięki temu możliwe jest potwierdzenie tożsamości użytkownika oraz ochrona danych przesyłanych między aplikacją a serwerem.

## Jak

Żeby wysłać żądanie HTTP z uwierzytelnieniem podstawowym, należy najpierw utworzyć obiekt `XMLHttpRequest` i przypisać mu adres URL serwera, do którego chcemy się połączyć. Następnie należy ustawić metodę żądania (np. `GET` lub `POST`) oraz dodatkowe nagłówki, w tym nagłówek `Authorization`, zawierający dane uwierzytelniające.

```Javascript
var xhr = new XMLHttpRequest();
var url = "https://example.com/auth";
xhr.open('GET', url);
xhr.setRequestHeader('Authorization', 'Basic ' + btoa('username:password')); // btoa - koduje dane do postaci base64
xhr.send();
```

Jeśli uwierzytelnienie przebiegnie pomyślnie, otrzymamy odpowiedź z serwera, która może być w formacie JSON lub tekstowym. Przykład odpowiedzi z nagłówkiem `Authorization` zawierającym dane uwierzytelniające może wyglądać następująco:

```Javascript
{
  "success": true,
  "message": "Uwierzytelniono pomyślnie.",
  "user_id": 123,
  "Authorization": "Basic dXNlcm5hbWU6cGFzc3dvcmQ=" // dane uwierzytelniające zakodowane w base64
}
```

## Pogłębione zagadnienia

Wysyłanie żądania HTTP z uwierzytelnianiem podstawowym polega na przekazaniu danych uwierzytelniających w formacie base64 w nagłówku `Authorization`. Warto jednak pamiętać, że jest to słaby sposób uwierzytelnienia i zalecane jest stosowanie bardziej zaawansowanych mechanizmów, takich jak uwierzytelnianie oparte o tokeny.

## Zobacz także

- [MDN - Wysyłanie żądań HTTP z XMLHttpRequest](https://developer.mozilla.org/pl/docs/Web/API/XMLHttpRequest)
- [MDN - Nagłówki HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP/Headers)
- [W3Schools - Uwierzytelnianie HTTP](https://www.w3schools.com/tags/att_http_equiv.asp)