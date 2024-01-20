---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem za pomocą TypeScript

## Co i dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to sposób na przesyłanie danych między klientem a serwerem, używając identyfikacji użytkownika i hasła zakodowanych w formacie Base64. Programiści to robią, aby zabezpieczyć dostęp do zasobów przed nieautoryzowanym dostępem.

## Jak to zrobić:
W TypeScript możemy skorzystać z biblioteki "axios" do wysyłania żądań HTTP. Poniżej przykład:

```TypeScript
import axios from 'axios';

async function sendRequest() {
    const response = await axios({
        method: 'get',
        url: 'http://example.com',
        auth: {
            username: 'user',
            password: 'pass'
        }
    });

    console.log(response.data);
}

sendRequest();
```

## Pogłębione spojrzenie
Podstawowe uwierzytelnianie HTTP, które jest stosunkowo starym systemem uwierzytelniania, uchodzi za proste w implementacji, ale nie jest to metoda bezpieczna lub optymalna, szczególnie gdy dane są przesyłane przez nieszyfrowane połączenia. Z tego powodu wiele aplikacji webowych zdecydowało się na użycie innych metod uwierzytelniania, takich jak OAuth lub JSON Web Tokens (JWT).

Ostateczne wdrożenie tej techniki w TypeScript zależy od biblioteki, z której korzystasz do obsługi żądań HTTP. W przypadku "axios", wykorzystujemy obiekt "auth" do przekazania identyfikacji i hasła.

## Zobacz też:
* [Podstawy Uwierzytelniania HTTP - MDN](https://developer.mozilla.org/pl/docs/Web/HTTP/Authentication)
* [Wprowadzenie do JSON Web Tokens](https://jwt.io/introduction/)