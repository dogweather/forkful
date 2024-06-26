---
date: 2024-01-20 18:00:46.493299-07:00
description: "How to: (Jak to zrobi\u0107:) Wysy\u0142anie \u017C\u0105da\u0144 HTTP\
  \ ma d\u0142ug\u0105 histori\u0119, ukorzenion\u0105 w pocz\u0105tkach internetu,\
  \ kiedy to Tim Berners-Lee wynalaz\u0142 HTTP. Alternatywnie\u2026"
lastmod: '2024-04-05T22:50:49.439112-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) Wysy\u0142anie \u017C\u0105da\u0144 HTTP ma d\u0142\
  ug\u0105 histori\u0119, ukorzenion\u0105 w pocz\u0105tkach internetu, kiedy to Tim\
  \ Berners-Lee wynalaz\u0142 HTTP."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

## How to: (Jak to zrobić:)
```TypeScript
import axios from 'axios';

// GET request do pobrania danych użytkownika
axios.get('https://jsonplaceholder.typicode.com/users/1')
  .then(response => {
    console.log(response.data);
  })
  .catch(error => console.error('There was an error!', error));

// POST request do stworzenia nowego użytkownika
axios.post('https://jsonplaceholder.typicode.com/users', {
  name: 'Jan Kowalski',
  username: 'jankowalski',
})
.then(response => {
  console.log('User created:', response.data);
})
.catch(error => console.error('Error creating user', error));
```

Sample output:
```
User created: {name: 'Jan Kowalski', username: 'jankowalski', id: 11}
```

## Deep Dive (Głębsze zanurzenie)
Wysyłanie żądań HTTP ma długą historię, ukorzenioną w początkach internetu, kiedy to Tim Berners-Lee wynalazł HTTP. Alternatywnie do axios, używać można Fetch API – natywna funkcja JavaScriptu. Axios oferuje jednak więcej funkcji, np. automatyczną transformację JSON. Wysyłając żądanie, należy zrozumieć metody takie jak GET, POST, a także statusy odpowiedzi HTTP (np. 200 oznacza sukces, a 404 - nie znaleziono zasobu).

## See Also (Zobacz także)
- [MDN Web Docs on HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP)
- [Axios documentation](https://axios-http.com/docs/intro)
- [Fetch API on MDN Web Docs](https://developer.mozilla.org/pl/docs/Web/API/Fetch_API)
- [Understanding REST APIs](https://restfulapi.net/)
