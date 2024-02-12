---
title:                "Wysyłanie żądania HTTP"
aliases:
- /pl/javascript/sending-an-http-request.md
date:                  2024-01-20T18:00:17.160766-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie zapytania HTTP to sposób, w jaki nasz kod komunikuje się z serwerami internetowymi - pobiera dane, wysyła informacje, aktualizuje zawartość. Programiści robią to, by ich aplikacje mogły interakcyjnie współdziałać z zewnętrznymi systemami i usługami.

## Jak to zrobić:

Aby wysłać zapytanie HTTP w JavaScript, możemy użyć wbudowanego interfejsu `fetch` albo starszego `XMLHttpRequest`. Oto przykład użycia fetch:

```javascript
// GET request używając fetch()
fetch('https://api.example.com/data')
  .then(response => {
    if (!response.ok) {
      throw new Error('Network response was not ok ' + response.statusText);
    }
    return response.json();
  })
  .then(data => console.log(data))
  .catch(error => console.error('There has been a problem with your fetch operation:', error));

// POST request używając fetch()
fetch('https://api.example.com/data', {
  method: 'POST',
  headers: {
    'Content-Type': 'application/json',
  },
  body: JSON.stringify({ key: 'value' }),
})
  .then(response => response.json())
  .then(data => console.log('Success:', data))
  .catch((error) => console.error('Error:', error));
```

Wynik:
```
Success: { ... odpowiedź serwera ... }
Error: ... informacje o błędzie ...
```

## Deep Dive

Wczesne metody AJAX, jak `XMLHttpRequest`, były standardem przez lata, ale miały swoje wady - brak obietnic (promises), skomplikowany API. `fetch` pojawił się w języku JavaScript, by to ułatwić, oferuje prostszy sposób na asynchroniczne żądania HTTP i obsługuje obietnice.

Alternatywnie, w Node.js i niektórych aplikacjach klienckich możemy użyć biblioteki `axios`. Jest ona oparta na `promises` i często uważana za bardziej czytelną:

```javascript
const axios = require('axios');

// Przykład z axios
axios.get('https://api.example.com/data')
  .then(response => console.log(response.data))
  .catch(error => console.log(error));
```

Warto wiedzieć, że każdy z tych sposobów radzi sobie z różnymi aspektami bezpieczeństwa, jak polityka CORS, różnią się także w obsłudze przerywania zapytań i streamingu.

## Zobacz także

- Dokumentacja MDN dla `fetch`: [MDN - Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API)
- Dokumentacja dla `axios`: [Axios on GitHub](https://github.com/axios/axios)
- Przewodnik po CORS: [MDN - CORS](https://developer.mozilla.org/en-US/docs/Web/HTTP/CORS)
