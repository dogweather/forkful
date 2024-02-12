---
title:                "Einen HTTP-Request senden"
aliases:
- de/php/sending-an-http-request.md
date:                  2024-01-20T18:00:06.622860-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen sind der Weg, wie deine Webseite oder Anwendung mit anderen Servern und APIs spricht. Wir nutzen sie, um Daten zu holen, zu senden oder Dienste von externen Quellen zu nutzen.

## How to:
In PHP nutzen wir cURL oder Streams, um HTTP-Anfragen zu senden. Hier ist ein PHP-Snippet, das zeigt, wie man eine GET-Anfrage mit cURL macht:

```PHP
<?php
$curl = curl_init();

curl_setopt($curl, CURLOPT_URL, "http://example.com");
curl_setopt($curl, CURLOPT_RETURNTRANSFER, 1);

$response = curl_exec($curl);
if ($response === false) {
    echo 'cURL Error: ' . curl_error($curl);
} else {
    echo 'Antwort erhalten: ' . $response;
}

curl_close($curl);
?>
```

Output:
```
Antwort erhalten: [Hier kommt die Antwort des Servers]
```

Und so sieht's mit Streams aus:

```PHP
<?php
$options = [
    'http' => [
        'method'  => 'GET',
        'header'  => 'Accept: application/json'
    ]
];
$context = stream_context_create($options);
$response = file_get_contents('http://example.com', false, $context);

if ($response) {
    echo 'Antwort erhalten: ' . $response;
} else {
    echo 'Fehler beim Holen der Daten.';
}
?>
```

Output:
```
Antwort erhalten: [Hier kommt die Antwort des Servers]
```

## Deep Dive
Vor langer Zeit war’s komplizierter, HTTP-Anfragen zu senden. Man musste per Hand Sockets öffnen und das HTTP-Protokoll zeilenweise schreiben. Mit der Zeit hat PHP das vereinfacht. cURL ist seit PHP v4.0.2 verfügbar, aber erst seit v5.0.0 ist das Arbeiten mit HTTP-Streams einfacher geworden.

Alternativen zu cURL und Streams sind fertige Bibliotheken wie Guzzle, die noch komfortablere Schnittstellen bieten, aber zusätzliche Abhängigkeiten bedeuten.

Wenn du cURL benutzt, musst du dich mit verschiedenen Optionen auseinandersetzen wie Timeouts, User Agents und Header. Streams sind einfacher, aber nicht so mächtig. Denk an die Sicherheit bei beiden Ansätzen – zum Beispiel solltest du niemals SSL-Zertifikate ohne Überprüfung akzeptieren.

## See Also
- [PHP Manual on HTTP Contexts](https://www.php.net/manual/en/context.http.php)
- [PHP cURL Manual](https://www.php.net/manual/en/book.curl.php)
- [Guzzle, PHP HTTP Client](http://docs.guzzlephp.org/en/stable/)
- [Requests for PHP](https://requests.ryanmccue.info/)
