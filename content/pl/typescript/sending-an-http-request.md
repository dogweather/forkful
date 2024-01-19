---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP to jeden ze sposobów, jaki komputery wymieniają dane między sobą. Programiści korzystają z tego do komunikacji z serwerami, aby na przykład zapisywać dane, otrzymywać informacje czy usuwać zasoby.

## Jak to zrobić:
W TypeScript możemy skorzystać z wbudowanej funkcji fetch, aby wysłać żądanie HTTP. Poniżej znajduje się przykład, jak to zrobić.

```TypeScript
fetch('https://api.github.com/users/github')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.log('Błąd:', error));
```

Po uruchomieniu powyższego kodu, zobaczysz wynik w konsoli, który będzie zawierać dane zwrócone z API Github.

## Więcej szczegółów
Kiedy przychodzi do wysyłanaia żądań HTTP, TypeScript jest dość elastyczny. Możemy korzystać z funkcji fetch, która jest obecnie najpopularniejszą metodą wysyłania żądań HTTP, ale nie jest jedyną możliwością. Alternatywą jest używanie bibliotek zewnętrznych jak Axios.

Pierwotnie, żądania HTTP były wykorzystywane tylko w przeglądarkach internetowych. Ale z powodu ich niezawodności, szybko zyskały popularność wśród developerów i teraz są standardem w komunikacji między komputerami.

Więcej informacji o wykonaniu detalach takich jak zrozumienie, co to jest promise czy jak działa Async/Await w TypeScript znajdziesz tutaj: [Mozilla Developer Network](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Using_promises)

## Zobacz także
Sprawdź te źródła, aby nauczyć się więcej na temat wysyłania żądań HTTP w TypeScript:

- [MDN Web Docs - Fetch API](https://developer.mozilla.org/pl/docs/Web/API/Fetch_API)
- [Axios - Promise based HTTP client](https://axios-http.com/)
- [HTTP request methods - MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)
- [TypeScript Deep Dive - AJAX Request](https://basarat.gitbook.io/typescript/nodejs/http)