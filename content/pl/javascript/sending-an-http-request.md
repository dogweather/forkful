---
title:                "Wysyłanie żądania HTTP"
html_title:           "Javascript: Wysyłanie żądania HTTP"
simple_title:         "Wysyłanie żądania HTTP"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Co i dlaczego?

Wysyłanie żądania HTTP to proces, w którym komputer wysyła prośbę o zasób do innej maszyny w sieci. Programiści często wykorzystują to w swoich projektach, ponieważ pozwala to na pobieranie danych z innych stron lub serwerów, co ułatwia tworzenie rozbudowanych aplikacji.

# Jak to zrobić:

```Javascript
const request = new XMLHttpRequest();
request.open('GET', 'https://jsonplaceholder.typicode.com/todos/1');
request.send();
```

W tym przykładzie wykorzystujemy obiekt XMLHttpRequest, który pozwala nam na wysłanie żądania GET do serwera podanego w drugim argumencie funkcji `open()`. Następnie wywołujemy funkcję `send()` aby wysłać żądanie i zapisać odpowiedź do obiektu `request`.

# Głębszy wgląd:

1. Kontekst historyczny: Wysyłanie żądań HTTP stało się możliwe dzięki rozwojowi oraz standardyzacji protokołu HTTP. Dzięki niemu możemy komunikować się z serwerem i pobierać potrzebne nam zasoby.

2. Alternatywy: Obecnie najpopularniejszą metodą wysyłania żądań HTTP jest wykorzystanie biblioteki `fetch` lub frameworków takich jak `Axios`.

3. Szczegóły implementacji: Wysyłanie żądań HTTP odbywa się przy użyciu różnych metod takich jak GET, POST, PUT czy DELETE. Każda z nich ma swoje zastosowanie i pozwala na różne operacje na zasobach.

# Zobacz również:

- Dokumentacja XmlHttpRequest: https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest
- Poradnik z wykorzystaniem `fetch`: https://www.sitepoint.com/fetch-api/
- Biblioteka Axios: https://axios-http.com/docs/intro