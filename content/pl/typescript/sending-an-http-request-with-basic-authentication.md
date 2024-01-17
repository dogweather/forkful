---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "TypeScript: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP z podstawową autoryzacją to sposób na przekazanie danych uwierzytelniających w celu uzyskania dostępu do zasobów internetowych. Programiści używają go, aby zapewnić bezpieczeństwo i ochronę swoich aplikacji i serwisów.

## Jak to zrobić:
```TypeScript
import axios from 'axios';

const username = 'example';
const password = 'password';

axios.get('https://example.com', {
  auth: {
    username,
    password
  }
})
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.log(error);
  });
```

## Głębszy zanurzenie:
Wysyłanie żądania HTTP z podstawową autoryzacją ma swoje korzenie w protokole HTTP z lat 90. Alternatywą dla tego rodzaju uwierzytelnienia jest autoryzacja z użyciem tokenów lub kluczy API. Implementacja polega na przesłaniu danych uwierzytelniających wraz z żądaniem HTTP poprzez nagłówek Authorization.

## Zobacz także:
- [Dokumentacja Axios](https://axios-http.com/docs/intro)
- [Porównanie różnych metod uwierzytelnienia w żądaniach HTTP](https://www.toptal.com/web/cookie-free-authentication-with-json-web-tokens-an-example-in-laravel-and-angularjs)