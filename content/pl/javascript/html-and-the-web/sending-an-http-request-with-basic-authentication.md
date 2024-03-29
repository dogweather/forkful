---
date: 2024-01-20 18:02:04.934666-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP z autoryzacj\u0105 podstawow\u0105\
  \ to proces, w kt\xF3rym przesy\u0142amy nasze dane uwierzytelniaj\u0105ce (login\
  \ i has\u0142o) w nag\u0142\xF3wku \u017C\u0105dania HTTP, aby\u2026"
lastmod: '2024-03-13T22:44:35.796293-06:00'
model: gpt-4-1106-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP z autoryzacj\u0105 podstawow\u0105\
  \ to proces, w kt\xF3rym przesy\u0142amy nasze dane uwierzytelniaj\u0105ce (login\
  \ i has\u0142o) w nag\u0142\xF3wku \u017C\u0105dania HTTP, aby\u2026"
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP z autoryzacją podstawową to proces, w którym przesyłamy nasze dane uwierzytelniające (login i hasło) w nagłówku żądania HTTP, aby uzyskać dostęp do zasobów wymagających autoryzacji. Programiści stosują to do bezpiecznej komunikacji z API, które wymaga uwierzytelniania.

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
