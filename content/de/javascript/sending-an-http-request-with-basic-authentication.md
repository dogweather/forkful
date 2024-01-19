---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# HTTP Anfrage mit Basic Authentication senden: Was und warum?

## Was & Warum?

Die HTTP Anfrage mit Basic Authentication ist eine Technik, um sicheren Zugang zu Webressourcen zu gewährleisten. Programmierer verwenden es, um sicher zu stellen, dass nur berechtigte Benutzer Zugriff auf bestimmte Ressourcen haben.

## Wie es geht:

`axios` ist eine beliebte Javascript-Bibliothek, um HTTP-Anfragen zu erstellen. Hier ist ein einfacher Code, um eine HTTP-Anfrage mit Basic Authentication zu erstellen:

```Javascript
const axios = require("axios");

axios({
    method: "get",
    url: "http://api.example.com/data",
    auth: {
        username: "Benutzername",
        password: "Passwort"
    }
})
.then(response => {
    console.log(response.data);
})
.catch(error => {
    console.log(error);
});
```

Wenn dieses Script ausgeführt wird, werden Ihre Benutzername und Passwort Base64-codiert und im Authorization Header der Anfrage hinzugefügt.

## Deep Dive

Historisch gesehen wurde die Basic Authentication Methode bereits in den frühen Tagen des Internets eingeführt. Sie ist jedoch aufgrund ihrer schwach ausgeprägten Sicherheitsmechanismen inzwischen veraltet.

Alternativen zur Basic Authentication sind Token-basierte Authentifizierungsmethoden wie Bearer Token oder OAuth2. Diese bieten eine höhere Sicherheit, da sie einen Einmal-Token anstelle von Benutzername und Passwort verwenden.

Beim Senden einer HTTP-Anfrage mit Basic Authentication sollten Sie vorsichtig sein, da die übertragenen Daten nicht verschlüsselt sind. Für höhere Sicherheit können Sie Ihre Anfragen über HTTPS senden.

## Siehe auch

Für weitere Informationen, überprüfen Sie diese Links:

1. [Axios Dokumentation](https://axios-http.com/)
2. [Mozilla’s HTTP Authentication Guide](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
3. [NPM Paket für Basic-Auth](https://www.npmjs.com/package/basic-auth)