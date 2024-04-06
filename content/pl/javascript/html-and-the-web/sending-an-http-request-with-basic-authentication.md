---
date: 2024-01-20 18:02:04.934666-07:00
description: "Jak to zrobi\u0107: Przykladowy wynik."
lastmod: '2024-04-05T21:53:37.223218-06:00'
model: gpt-4-1106-preview
summary: ''
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## Jak to zrobić:
```javascript
const axios = require('axios');
const base64 = require('base-64');

const username = 'twojanazwa';
const password = 'twojehaslo';
const basicAuth = 'Basic ' + base64.encode(username + ':' + password);

axios.get('https://twojapi.pl/dane', { headers: { Authorization: basicAuth } })
  .then(response => {
    console.log('Dostęp uzyskany:', response.data);
  })
  .catch(error => {
    console.error('Błąd autoryzacji:', error);
  });
```
Przykladowy wynik:
```
Dostęp uzyskany: { "tajneDane": "bardzo tajne" }
```

## Głębsze zanurzenie
Podstawowa autoryzacja HTTP to stary, ale prosty sposób na zabezpieczenie dostępu do zasobów. Wysyłasz login i hasło zakodowane w Base64 w nagłówku żądania. Mimo że łatwa w implementacji, ma swoje wady – głównie niski poziom bezpieczeństwa. Dane są łatwe do odczytania, jeśli ktoś przechwyci ruch sieciowy. Dlatego często używa się jej z HTTPS, co zapewnia szyfrowane połączenie.

Alternatywą dla podstawowej autoryzacji jest na przykład OAuth, który jest bezpieczniejszy, ale także bardziej skomplikowany w implementacji. JWT (JSON Web Tokens) także zyskuje popularność jako sposób na autoryzację i wymianę informacji, szczególności w aplikacjach SPA (Single Page Application).

Ważne jest, aby pamiętać, że każda metoda uwierzytelniania ma swoje plusy i minusy w zależności od konkretnego przypadku użycia, bezpieczeństwa i łatwości implementacji.

## Zobacz także
- [MDN Web Docs – Autoryzacja podstawowa HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP/Authentication)
- [Axios – Promise based HTTP client for the browser and node.js](https://github.com/axios/axios)
