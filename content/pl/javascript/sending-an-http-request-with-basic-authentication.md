---
title:                "Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem."
html_title:           "Javascript: Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem."
simple_title:         "Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co & Dlaczego?

Wysyłanie zapytania HTTP z podstawowym uwierzytelnianiem to proces, w którym programista wyśle żądanie do serwera internetowego, aby uzyskać dostęp do chronionego zasobu. Uwierzytelnienie zapewnia bezpieczne i poufne wykorzystywanie zasobu i jest niezbędne do zapewnienia bezpieczeństwa danych.

## Jak to zrobić?

Przykładowy kod w JavaScript, wykorzystujący bibliotekę Axios, do wysyłania żądań HTTP z podstawowym uwierzytelnieniem:

```
const axios = require('axios');

axios.get('/protected/resource', {
  auth: {
    username: 'username',
    password: 'password'
  }
})
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.log(error);
  });
```

Przykładowy wynik zwrócony przez serwer:

```
{
  data: "Dane zasobu"
}
```

## Głębszy zanurzenie

Podstawowe uwierzytelnienie zostało wprowadzone w protokole HTTP w celu połączenia użytkownika z serwerem. Alternatywą dla tego typu uwierzytelnienia jest wykorzystanie tokenów, które są bezpieczniejszym sposobem uwierzytelniania i są często stosowane w API.

Implementacja zapytań HTTP z podstawowym uwierzytelnieniem może różnić się w zależności od wykorzystywanej biblioteki lub frameworka. Przykład powyżej wykorzystuje bibliotekę Axios, ale istnieją też inne sposoby wykonywania tego typu żądań.

## Zobacz również

- [Axios dokumentacja](https://axios-http.com/docs/intro)
- [HTTP Basic Authentication: co to jest i jak działa](https://www.w3schools.com/tags/att_input_type_button.asp)