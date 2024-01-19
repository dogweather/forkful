---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen mit Basic-Authentifizierung lassen den Client dem Server seine Identität durch Benutzername und Passwort in der Anfrage bestätigen. Programmierer nutzen dies für sichere Datenübertragung zwischen Server und Client.

## Wie geht's:
Hier ist ein einfacher PHP-Code, der eine HTTP-Anfrage mit Basic-Authentifizierung sendet:

```PHP
<?php
$options = [
    'http' => [
        'header' => "Authorization: Basic " . base64_encode("username:password")
    ]
];
$context = stream_context_create($options);
$resultat = file_get_contents('http://example.com', false, $context);
?>
```

Die Antwort vom Server, `$resultat`, enthält dann die vom Server zurückgesandten Daten.

## Tiefgehende Informationen:
Historisch gesehen wurde HTTP Basic Auth als Teil des ursprünglichen HTTP-Standards eingeführt, um einen simplen Authentifizierungsmechanismus zu bieten. Es bietet zwar keine Verschlüsselung, kann aber bei korrekter Verwendung mit SSL/TLS effektiv sein.

Alternativen zur HTTP Basic Auth umfassen Digest-Authentifizierung, OAuth und JWT (JSON Web Tokens). Alle bieten unterschiedliche Sicherheitsstufen und Flexibilität.

Ein wichtiger Aspekt der HTTP Basic Auth in PHP ist die Verarbeitung über die `stream_context_create` Funktion. Dies erstellt einen "Kontext" - eine Sammlung von Optionen -  der dann von `file_get_contents` verwendet wird, um die Daten abzurufen.

## Siehe auch:
- PHP-Dokumentation zu stream_context_create: https://www.php.net/manual/de/function.stream-context-create.php
- Mehr über HTTP Basic Auth: https://tools.ietf.org/html/rfc7617
- PHP-Dokumentation zu file_get_contents: https://www.php.net/manual/de/function.file-get-contents.php

Als nächstes empfehle ich, mehr über die alternativen Authentifizierungsmethoden zu lernen und sie gegebenenfalls in Ihrer zukünftigen Arbeit zu implementieren.