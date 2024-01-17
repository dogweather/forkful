---
title:                "Versenden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "TypeScript: Versenden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Versenden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/typescript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Was ist es und warum?

Das Senden von HTTP-Anfragen mit Basic Authentication ist ein gängiger Prozess in der Webentwicklung, bei dem ein Benutzername und ein Passwort verwendet werden, um auf eine gesicherte Ressource zuzugreifen. Programmierer nutzen diese Methode, um die Sicherheit ihrer Anwendungen zu verbessern und nur autorisierten Benutzern Zugriff zu gewähren.

Wie geht es?

***Hinweis:*** Für die folgenden Beispiele werden die `fetch`-Funktion und die im Browser integrierte FetchAPI verwendet. Diese können in anderen Umgebungen wie Node.js durch ähnliche Methoden ersetzt werden.

**1. Senden einer einfachen GET-Anfrage mit Basic Authentication:**

```
fetch("https://meinewebseite.de/api/meineDaten", {
  headers: {
    Authorization: "Basic base64EncodedCredentials"
  }
})
  .then(response => response.json())
  .then(data => console.log(data));
```

**2. Senden einer POST-Anfrage mit Basic Authentication und Übergeben von Daten:**

```
fetch("https://meinewebseite.de/api/benutzer", {
  method: "POST",
  headers: {
    Authorization: "Basic base64EncodedCredentials",
    "Content-Type": "application/json"
  },
  body: JSON.stringify({ name: "Max Mustermann", email: "max@beispiel.com" })
})
  .then(response => response.json())
  .then(data => console.log(data));
```

**Ausgabe:** Die Antwort der Serveranfrage wird in JSON-Format ausgegeben und kann dann in der Konsole ausgegeben oder weiterverarbeitet werden.

Tiefe Einblicke

In der Frühphase des Internets wurde Basic Authentication als eine der einfachsten Methoden zur Authentifizierung von Benutzern entwickelt. Es ist jedoch nicht die sicherste Methode, da Benutzername und Passwort im Klartext übertragen werden. Es gibt auch alternative Methoden wie OAuth, die eine sicherere Form der Authentifizierung ermöglichen.

Die Implementierung von HTTP-Anfragen mit Basic Authentication erfordert die Verwendung von Base64-Verschlüsselung für die Anmeldeinformationen, die dann im Header der Anfrage gesendet werden. Der Server decodiert diese Informationen und authentifiziert den Benutzer auf Basis der übergebenen Daten.

Siehe auch

- [Artikel über Basic Authentication](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication#Basic_authentication_scheme)
- [Alternative Methoden zur Authentifizierung im Web](https://www.keycdn.com/blog/http-security-headers)
- [Dokumentation zu FetchAPI](https://developer.mozilla.org/de/docs/Web/API/Fetch_API)