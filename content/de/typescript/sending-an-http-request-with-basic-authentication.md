---
title:                "TypeScript: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum 

In der modernen Webentwicklung ist der Austausch von Daten zwischen verschiedenen Systemen unerlässlich. Eine gängige Methode, um dies zu erreichen, ist die Verwendung von HTTP-Anfragen. Manchmal ist es jedoch notwendig, dass die Anfrage durch eine Authentifizierung geschützt wird, um sicherzustellen, dass nur autorisierte Benutzer Zugriff auf die Daten haben. In diesem Fall wird die Verwendung von grundlegender Authentifizierung empfohlen, die eine Benutzeranmeldung erfordert, um auf die Ressourcen zugreifen zu können.

## Wie funktioniert es

Um eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden, müssen wir zuerst die erforderlichen Daten wie URL, Methode und Header festlegen. Dann fügen wir den Benutzernamen und das Passwort im Base64-Format zum Header hinzu. Hier ist ein Beispiel in TypeScript:

```TypeScript
import axios from 'axios';

// Definieren der URL, Methode und Header
const url = 'https://myapp.com/api/users';
const method = 'GET';
const headers = {
  'Authorization': 'Basis ' + btoa('Benutzername:Passwort')
}

// Senden der Anfrage mit axios
axios({
  method: method,
  url: url,
  headers: headers
}).then(response => {
  // Ausgabe der erhaltenen Daten
  console.log(response.data);
}).catch(error => {
  // Fehlerbehandlung
  console.log(error);
});
```

Der Header enthält den Schlüssel "Authorization" und den Wert "Basic", gefolgt von einem Leerzeichen und dem Base64-kodierten Benutzernamen und Passwort im folgenden Format: "Benutzername:Passwort". Dadurch wird der Benutzer beim Server authentifiziert und die Anfrage kann weitergeleitet werden.

Ein Beispiel für den empfangenen Output kann wie folgt aussehen:

```json
{
  "id": 123,
  "username": "beispielbenutzer",
  "email": "beispiel@meinapp.com"
}
```

## Tiefere Einblicke

Es gibt einige wichtige Dinge zu beachten, wenn man eine HTTP-Anfrage mit grundlegender Authentifizierung sendet. Zum Beispiel sollte man sicherstellen, dass das Passwort vor dem Codieren mit Base64 verschlüsselt wird, um sicherzustellen, dass es nicht im Klartext übertragen wird. Außerdem sollte man beachten, dass grundlegende Authentifizierung nicht die sicherste Methode ist, da die Anmeldeinformationen im Header sichtbar sind. In sicherheitskritischen Anwendungen ist es daher möglicherweise besser, eine andere Methode zu verwenden, wie z.B. die Verwendung von OAuth.

## Siehe auch

- The Ultimate Guide to Basic Authentication in TypeScript - https://blog.logrocket.com/basic-authentication-in-typescript
- Axios documentation - https://github.com/axios/axios
- Base64 Encoding in TypeScript - https://www.w3schools.com/jsref/met_win_atob.asp