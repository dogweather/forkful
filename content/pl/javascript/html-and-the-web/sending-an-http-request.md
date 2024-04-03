---
date: 2024-01-20 18:00:17.160766-07:00
description: "Jak to zrobi\u0107: Aby wys\u0142a\u0107 zapytanie HTTP w JavaScript,\
  \ mo\u017Cemy u\u017Cy\u0107 wbudowanego interfejsu `fetch` albo starszego `XMLHttpRequest`.\
  \ Oto przyk\u0142ad u\u017Cycia\u2026"
lastmod: '2024-03-13T22:44:35.793306-06:00'
model: gpt-4-1106-preview
summary: "Aby wys\u0142a\u0107 zapytanie HTTP w JavaScript, mo\u017Cemy u\u017Cy\u0107\
  \ wbudowanego interfejsu `fetch` albo starszego `XMLHttpRequest`."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

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
