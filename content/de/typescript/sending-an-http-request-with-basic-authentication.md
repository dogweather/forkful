---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# HTTP-Anfragen mit Basic-Authentifizierung in TypeScript

## Was & Warum?

Eine HTTP-Anfrage mit Basic-Authentifizierung ist eine Methode zur Übertragung von Anmeldeinformationen (Benutzername und Passwort) über das Internet. Programmierer verwenden sie, um sicheren Zugang zu Web-APIs zu ermöglichen.

## Wie geht das?

Installiere zunächst das Axios, eine beliebte Bibliothek zur Durchführung von HTTP-Anfragen.

```TypeScript
npm install axios
```

Wir senden dann eine HTTP-GET-Anfrage mit Basic-Authentifizierung.

```TypeScript
import axios from 'axios';

const config = {
  auth: {
    username: 'Benutzername hier einfügen',
    password: 'Passwort hier einfügen'
  }
}

axios.get('https://meine-api.de', config)
  .then(response => console.log(response.data))
  .catch(error => console.error(error));
```

## Tiefentauchgang

Die Basic-Authentifizierung wurde von den frühen Standards des World Wide Web übernommen. Sie ist zwar einfach zu implementieren, bietet jedoch keine starke Sicherheit. Daher sollte sie über HTTPS und nicht über HTTP verwendet werden, um die Anmeldeinformationen zu verschlüsseln.

Alternativen zur Basic-Authentifizierung umfassen OAuth2 und JWT (JSON Web Token), die in Situationen mit höherer Sicherheitsanforderung vorteilhafter sind.

Die Zwei wichtigsten Teile der Implementierung von HTTP-Anfragen mit Basic-Authentifizierung sind: Erstens, der Parameter `auth` in der Axios-Anforderung, die den Benutzernamen und das Passwort enthält. Zweitens, die URL, auf die die Anfrage gerichtet ist. Diese Teile werden in der HTTP-Anforderung zusammengefügt und an den Server gesendet.

## Siehe auch

* [Axios-Dokumentation (englisch)](https://axios-http.com/docs/intro)
* [Grundlagen zur Basic-Authentifizierung (englisch)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)