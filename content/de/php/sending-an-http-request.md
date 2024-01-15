---
title:                "Senden einer HTTP-Anfrage"
html_title:           "PHP: Senden einer HTTP-Anfrage"
simple_title:         "Senden einer HTTP-Anfrage"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit dem Versenden von HTTP-Anfragen befassen? Nun, HTTP-Anfragen sind ein wesentlicher Bestandteil des World Wide Web und ermöglichen es uns, mit Websites und APIs zu kommunizieren. Sie sind essenziell für die Funktionalität von Webanwendungen und ermöglichen es uns, Daten von externen Quellen abzurufen oder zu senden.

## Wie geht das?

Das Senden von HTTP-Anfragen kann mit Hilfe von PHP auf einfache Weise erfolgen. Zunächst muss die URL der gewünschten Ressource angegeben werden, gefolgt von der Art der Anfrage (GET, POST, PUT, DELETE). Hier ist ein Beispiel, wie man mit PHP eine GET-Anfrage an eine API sendet:

```PHP
$url = "https://example.com/api/users"; // URL der Ressource
$response = file_get_contents($url); // GET-Anfrage ausführen
var_dump($response); // Ausgabe der erhaltenen Daten
``` 

In diesem Beispiel verwenden wir die `file_get_contents()` Funktion, um die Anfrage auszuführen. Diese Funktion gibt uns den Inhalt der erhaltenen Antwort als String zurück. In der Regel möchten wir jedoch mit den Daten arbeiten und sie beispielsweise als JSON entschlüsseln. Hier ist ein Beispiel, wie das gemacht werden kann:

```PHP
$url = "https://example.com/api/users";
$response = file_get_contents($url);
$data = json_decode($response, true); // Option true, um ein Array statt eines Objekts zu erhalten
var_dump($data); // Ausgabe der erhaltenen Daten als Array
```

Natürlich gibt es auch andere Möglichkeiten, HTTP-Anfragen in PHP zu senden, wie z.B. die `curl` Erweiterung oder die `fopen()` Funktion. Es ist wichtig zu beachten, dass beim Senden von HTTP-Anfragen auch die Verarbeitung von Fehlern und die Sicherheit berücksichtigt werden müssen.

## Deep Dive

Wenn wir tiefer in das Thema eintauchen, werden wir feststellen, dass HTTP-Anfragen aus mehreren Teilen bestehen. Dazu gehören der Request-Header, der die Informationen über die Anfrage enthält, und der Request-Body, der die eigentlichen Daten enthält. Außerdem gibt es verschiedene Arten von Anfragen, die für unterschiedliche Zwecke verwendet werden können.

Hier sind einige wichtige Dinge, die man bei der Arbeit mit HTTP-Anfragen beachten sollte:

- Wir sollten immer sicherstellen, dass wir die richtige HTTP-Methode für unsere Anfrage verwenden. In der Regel werden GET-Anfragen verwendet, um Daten abzurufen, POST-Anfragen, um Daten zu senden, PUT-Anfragen, um Daten zu aktualisieren, und DELETE-Anfragen, um Daten zu löschen.
- Die Art des Inhalts, der in der Anfrage übertragen wird, sollte ebenfalls angegeben werden. Zum Beispiel `Content-Type: application/json`, wenn der Request-Body ein JSON-Dokument enthält.
- Der Response-Code gibt an, ob die Anfrage erfolgreich war oder nicht. Eine 2xx-Statuscode zeigt an, dass die Anfrage erfolgreich war, während eine 4xx- oder 5xx-Code auf einen Fehler hinweist.
- Es ist wichtig, sich über Sicherheitsaspekte wie Cross-Site Request Forgery (CSRF) im Klaren zu sein und geeignete Maßnahmen zu ergreifen, um diese zu verhindern.

## Siehe auch

- [PHP: cURL - Dokumentation](https://www.php.net/manual/de/book.curl.php)
- [PHP: fopen - Dokumentation](https://www.php.net/manual/de/function.fopen.php)
- [HTTP Anfragen mit PHP - Codebeispiele](http://codebeispiele.de/http-anfragen-mit-php/)