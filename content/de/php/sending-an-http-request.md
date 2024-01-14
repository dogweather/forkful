---
title:                "PHP: Eine HTTP-Anfrage senden"
simple_title:         "Eine HTTP-Anfrage senden"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie eine Website besuchen, interagieren Sie wahrscheinlich mit verschiedenen Seiten und Inhalten. Hinter den Kulissen kommuniziert Ihr Browser mit Servern, um Inhalte und Daten abzurufen. Dieser Prozess wird durch das Senden von HTTP-Anfragen ermöglicht.

## Wie man eine HTTP-Anfrage sendet

Um eine HTTP-Anfrage zu senden, müssen Sie die Funktion `file_get_contents()` in PHP verwenden und die URL des Ziels sowie zusätzliche Optionen angeben. Zum Beispiel:

```PHP
<?php
$url = "https://www.meine-website.de";
$options = [
    'http' => [
        'method' => 'GET' // Erlaubt es uns, die Anfragemethode festzulegen, in diesem Fall GET
    ]
];

// Senden der Anfrage und Speichern der Antwort in einer Variablen
$response = file_get_contents($url, false, stream_context_create($options));

// Ausgabe der Antwort
echo $response;
?>
```

Dieses Beispiel zeigt, wie Sie eine einfache GET-Anfrage senden können. Sie können auch zusätzliche Optionen wie Benutzername und Passwort für eine Authentifizierung, Daten für eine POST-Anfrage und andere Header-Informationen hinzufügen. Weitere Informationen zu den verfügbaren Optionen finden Sie in der PHP-Dokumentation.

## Tiefergehende Informationen

Das Senden einer HTTP-Anfrage umfasst mehr als nur die Verwendung der `file_get_contents()`-Funktion. Sie können auch cURL, eine Bibliothek für die Client-Seite von URL-Übertragungen, verwenden, um erweiterte Optionen und Funktionen zu erhalten.

Eine vollständige Erklärung der HTTP-Anfragen könnte den Rahmen dieses Blog-Posts sprengen, aber es ist wichtig zu wissen, dass eine Anfrage aus einem Request-Header, einem optionalen Request-Body und einer Antwort eines Servers besteht.

## Siehe auch

- [PHP-Dokumentation zu HTTP-Anfragen](https://www.php.net/manual/en/book.http.php)
- [cURL-Dokumentation](https://curl.se/)
- [Einführung in HTTP-Anfragen](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)