---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co to jest i dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem polega na przekazaniu danych logowania w nagłówku HTTP, aby potwierdzić tożsamość do serwera. Programiści robią to, aby chronić swoje dane przed niepowołanym dostępem.

## Jak to zrobić:
Oto prosty przykład kodu używającego Node.js do wysyłania żądania GET z podstawowym uwierzytelnieniem:

```Javascript
const http = require('http');

const options = {
  hostname: 'www.example.com',
  port: 80,
  path: '/',
  method: 'GET',
  headers: {
    'Authorization': 'Basic ' + new Buffer.from('username:password').toString('base64')
  }
};

http.request(options, (res) => {
  res.on('data', (d) => {
    process.stdout.write(d);
  });
}).end()
```

Wynik może wyglądać tak:

```Javascript
...
<div>Witaj na stronie przykładowej</div>
...
```

## Pogłębiona analiza
Historia: Kiedy Internet powstawał, podstawowe uwierzytelnianie było jednym z pierwszych mechanizmów uwierzytelniania zaimplementowanych w protokole HTTP.

Alternatywy: Chociaż podstawowe uwierzytelnianie jest proste w użyciu, nie jest szczególnie bezpieczne i zwykle jest zastępowane przez bezpieczniejsze metody, takie jak uwierzytelnianie oparte na tokenu.

Szczegóły implementacji: Podstawowe uwierzytelnianie korzysta z nagłówka 'Authorization'. Wartość tego nagłówka jest schematem uwierzytelnienia ('Basic') poprowadzonym przez spację do ciągu base64 utworzonego z połączenia nazwy użytkownika i hasła.

## Zobacz też
- MDN Web Docs - podstawowe uwierzytelnianie: https://developer.mozilla.org/
- Node.js - moduł http: https://nodejs.org/api/http.html
- Npm - moduł request: https://www.npmjs.com/package/request