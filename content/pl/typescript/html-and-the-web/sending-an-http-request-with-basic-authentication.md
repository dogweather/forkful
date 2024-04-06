---
date: 2024-01-20 18:02:51.902928-07:00
description: "How to: (Jak to zrobi\u0107:) Wyj\u015Bcie zale\u017Cy od API, ale zobaczysz\
  \ odpowied\u017A w konsoli."
lastmod: '2024-04-05T21:53:36.579432-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Wyj\u015Bcie zale\u017Cy od API, ale zobaczysz odpowied\u017A\
  \ w konsoli."
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## How to: (Jak to zrobić:)
```TypeScript
import axios from 'axios';

async function fetchWithBasicAuth(url: string, username: string, password: string) {
  const response = await axios.get(url, {
    auth: {
      username: username,
      password: password,
    },
  });
  return response.data;
}

// Użycie funkcji - wpisz swoje dane zamiast 'twojaNazwa' i 'twojeHasło'.
fetchWithBasicAuth('https://api.example.com/data', 'twojaNazwa', 'twojeHasło')
  .then(data => console.log(data))
  .catch(error => console.error('Error fetching data:', error));
```
Wyjście zależy od API, ale zobaczysz odpowiedź w konsoli.

## Deep Dive (Dogłębna Analiza)
Autoryzacja podstawowa (Basic Authentication) to stary, ale prosty sposób na uwierzytelnienie w protokole HTTP. Użytkownik przesyła nazwę i hasło zakodowane w base64 w nagłówku żądania. Mimo prostoty, metoda jest mniej bezpieczna niż nowsze mechanizmy, więc używaj przy zabezpieczonych połączeniach (HTTPS).

Alternatywą jest OAuth, tokeny JWT itp., które zapewniają większe bezpieczeństwo i elastyczność.

Detal implementacji: TypeScript (i JavaScript) nie mają wbudowanych funkcji do HTTP, więc często używa się bibliotek jak `axios`. Axios automatycznie zakoduje dane `username` i `password` i doda je do nagłówków.

## See Also (Zobacz również)
- Dokumentacja Axios: [https://github.com/axios/axios](https://github.com/axios/axios)
- Bezpieczeństwo Basic Authentication: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- O alternatywach: OAuth [https://oauth.net/](https://oauth.net/) i JWT [https://jwt.io/](https://jwt.io/)
