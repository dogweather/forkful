---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "TypeScript: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Warum: Warum sollte man sich mit dem Senden von HTTP-Anfragen mit grundlegender Authentifizierung beschäftigen? Es ist eine effektive Möglichkeit, sich bei einer Anwendung oder einem Service zu authentifizieren und sicherzustellen, dass nur autorisierte Benutzer darauf zugreifen können.

So geht's: Hier sind zwei einfache Beispiele, wie man eine HTTP-Anfrage mit grundlegender Authentifizierung in TypeScript sendet:

```
// Beispiel 1: Verwendung der XHR-Methode
function sendRequest(url: string, username: string, password: string) {
  let authHeader: string = "Basic " + btoa(username+":"+password);
  let xhr = new XMLHttpRequest();
  xhr.open("GET", url, false);
  xhr.setRequestHeader("Authorization", authHeader);
  xhr.send();
  console.log(xhr.responseText);
}

sendRequest("https://example.com/api/users", "benutzername123", "passwort123");

// Beispiel 2: Verwendung von axios
const axios = require('axios');

let authHeader = {
  username: "benutzername123",
  password: "passwort123"
}
let config = {
  headers: authHeader,
  method: 'get',
  url: 'https://example.com/api/users',
};

axios(config)
  .then(response => {
    console.log(response.data);
  })
  .catch(error => {
    console.log(error);
  });
```

Ausgabe Beispiel 1:
```
[
  {
    "id": "1234",
    "name": "Max Mustermann",
    "email": "max.mustermann@example.com"
  },
  {
    "id": "2345",
    "name": "Laura Schmidt",
    "email": "laura.schmidt@example.com"
  },
  {
    "id": "3456",
    "name": "Tom Wagner",
    "email": "tom.wagner@example.com"
  }
]
```

Ausgabe Beispiel 2:
```
[
  {
    "id": "1234",
    "name": "Max Mustermann",
    "email": "max.mustermann@example.com"
  },
  {
    "id": "2345",
    "name": "Laura Schmidt",
    "email": "laura.schmidt@example.com"
  },
  {
    "id": "3456",
    "name": "Tom Wagner",
    "email": "tom.wagner@example.com"
  }
]
```

Tiefgehende Einblicke: Bei der grundlegenden Authentifizierung wird der Username und das Passwort in Base64 kodierter Form als Teil des Authorization-Headers in der HTTP-Anfrage mitgeschickt. Dies bietet zwar eine Grundlage für die Authentifizierung, aber es ist nicht besonders sicher, da Base64 leicht entschlüsselt werden kann. Für eine höhere Sicherheit empfiehlt es sich, auf andere Authentifizierungsmethoden wie z.B. OAuth umzusteigen.

Siehe auch:
- [MDN Web Docs: HTTP Basic Authentication](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication#Basic_Authentication)
- [Axios: HTTP client for the browser and node.js](https://github.com/axios/axios)
- [Base64: encode/decode data in base64 format](https://github.com/niklasvh/base64-js)