---
title:                "Senden einer http-Anfrage mit grundlegender Authentifizierung"
html_title:           "PHP: Senden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Warum

PHP ist eine weit verbreitete und beliebte Programmiersprache für die Webentwicklung. Eine wichtige Aufgabe bei der Programmierung von Webanwendungen ist das Senden von HTTP-Anfragen. Mit der grundlegenden Authentifizierungsmethode von HTTP können Benutzer ihre Identität überprüfen, um auf geschützte Ressourcen zuzugreifen. In diesem Artikel werden wir darüber sprechen, warum Sie mit PHP HTTP-Anfragen mit grundlegender Authentifizierung senden sollten und wie Sie dies tun können.

# Wie es geht

Um eine HTTP-Anfrage mit grundlegender Authentifizierung zu senden, müssen Sie zunächst eine Instanz der `Curl`-Klasse erstellen. Diese Klasse bietet Methoden zum Ausführen von HTTP-Anfragen. Sie können dann die `setopt()`-Methode verwenden, um verschiedene Optionen für Ihre Anfrage festzulegen. Eine wichtige Option ist `CURLOPT_USERPWD`, die es Ihnen ermöglicht, einen Benutzernamen und ein Passwort für die grundlegende Authentifizierung anzugeben. Zum Beispiel:

```PHP
<?php

// Erstelle eine neue Instanz der Curl-Klasse
$ch = curl_init();

// Setze die URL, an die die Anfrage gesendet werden soll
curl_setopt($ch, CURLOPT_URL, 'http://example.com');

// Setze die Option für die grundlegende Authentifizierung
curl_setopt($ch, CURLOPT_USERPWD, 'username:password');

// Führe die Anfrage aus und speichere die Antwort in einer Variablen
$response = curl_exec($ch);

// Schließe die Curl-Sitzung
curl_close($ch);

// Gib die Antwort aus
echo $response;
```

Der oben gezeigte Code wird eine HTTP-GET-Anfrage an die angegebene URL senden und die Antwort in der `response`-Variablen speichern. Beachten Sie, dass Sie den Beitrag anpassen müssen, um Ihre gewünschten Benutzerdaten und die URL der Anfrage anzugeben.

# Tief eintauchen

Im obigen Beispiel haben wir das Passwort für die grundlegende Authentifizierung im Klartext angegeben. Dies ist jedoch keine sichere Methode, um sensible Informationen zu übertragen. Stattdessen sollten Sie eine Hash-Funktion wie `password_hash()` verwenden, um das Passwort zu verschlüsseln, bevor es an den Server gesendet wird. Der Server kann dann die Funktion `password_verify()` verwenden, um das Passwort mit dem gespeicherten Passwort zu vergleichen.

Eine weitere Möglichkeit, die grundlegende Authentifizierung sicherer zu machen, ist die Verwendung von HTTPS anstelle von HTTP. HTTPS verschlüsselt die Kommunikation zwischen dem Client und dem Server, so dass sensible Informationen nicht im Klartext übertragen werden.

# Siehe auch

- [PHP-Dokumentation zu "curl_setopt()"](https://www.php.net/manual/de/function.curl-setopt.php)
- [PHP-Dokumentation zu "password_hash()"](https://www.php.net/manual/de/function.password-hash.php)
- [PHP-Dokumentation zu "password_verify()"](https://www.php.net/manual/de/function.password-verify.php)
- [Informationen zu HTTPS von MDN Web Docs](https://developer.mozilla.org/de/docs/Web/HTTP/Overview)