---
title:                "Wysyłanie żądania http"
html_title:           "TypeScript: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

 Wysyłanie żądania HTTP jest niezwykle ważną częścią procesu tworzenia aplikacji internetowych. Pozwala ono na komunikację z serwerem i pobieranie danych, co jest niezbędne do działania wielu aplikacji.

## Jak to zrobić

```TypeScript
// Przykładowe żądanie GET przy użyciu biblioteki Axios
import axios from 'axios';

axios.get('https://api.example.com/users')
  .then((response) => {
    console.log(response.data);
  })
  .catch((error) => {
    console.log(error);
  });
```

```TypeScript
// Przykładowe żądanie POST przy użyciu wbudowanych metod języka TypeScript
const data = {
  email: 'example@gmail.com',
  password: 'secretpassword'
};

fetch('https://api.example.com/login', {
  method: 'POST',
  body: JSON.stringify(data)
})
  .then((response) => response.json())
  .then((data) => {
    console.log(data);
  })
  .catch((error) => {
    console.log(error);
  });
```

#### Szczegółowe informacje

Wysyłanie żądania HTTP jest możliwe dzięki wykorzystaniu różnych bibliotek i metod w języku TypeScript. Najpopularniejszymi bibliotekami do tego celu są Axios, Request czy Fetch API, które umożliwiają tworzenie żądań HTTP w prosty i efektywny sposób.

Przy wysyłaniu żądań należy zwrócić uwagę na kody odpowiedzi (status code), które dostarczają informacji o statusie wykonanego żądania. Dzięki nim można wykryć ewentualne błędy w komunikacji z serwerem i odpowiednio zareagować.

Wysyłając żądanie HTTP można także przesyłać dane w różnych formatach, np. jako dane JSON lub w formularzu. W takim przypadku należy odpowiednio sformatować ciało żądania przy użyciu metody JSON.stringify lub FormData.

## Zobacz także

- [Dokumentacja Axios](https://github.com/axios/axios)
- [Dokumentacja Fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- [Dokumentacja Request](https://github.com/request/request)