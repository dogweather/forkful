---
title:                "Versenden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "PHP: Versenden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Versenden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Was & Warum?

HTTP-Anfragen mit grundlegender Authentifizierung sind ein Weg, um sich bei einem Webdienst zu identifizieren und Zugriff zu erhalten. Programmierer nutzen dies, um Daten von einer externen Quelle abzurufen oder zu übertragen, beispielsweise in einer API-Anfrage oder beim Herunterladen von Dateien.

Wie geht's?

```PHP 
// Beispiel für eine HTTP-Anfrage mit grundlegender Authentifizierung 
$ch = curl_init(); 
curl_setopt($ch, CURLOPT_URL, "https://www.example.com/api/send_data"); 
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC); // HTTP-Authentifizierung verwenden 
curl_setopt($ch, CURLOPT_USERPWD, "username:password"); // Benutzername und Passwort eingeben 
$result = curl_exec($ch); // Anfrage ausführen 
curl_close($ch); // Verbindung schließen 
echo $result; // Ergebnis anzeigen 
```

> Ausgabe: Daten wurden erfolgreich übertragen.

Tiefer eintauchen

- Die grundlegende Authentifizierung wurde in den 1990er Jahren eingeführt und ist die einfachste Form der HTTP-Authentifizierung.
- Es gibt auch alternative Methoden wie Digest- und OAuth-Authentifizierung, die zusätzliche Sicherheitsmechanismen bieten.
- Die Implementierung einer HTTP-Anfrage mit grundlegender Authentifizierung erfordert das Einrichten von HTTP-Headern und das Übermitteln von Benutzername und Passwort.

Siehe auch

- [PHP cURL-Dokumentation](https://www.php.net/manual/de/book.curl.php) für weitere Informationen und Beispiele.
- [HTTP-Authentifizierung im Detail erklärt](https://developer.mozilla.org/de/docs/Web/HTTP/Authentication) auf MDN Web Docs.