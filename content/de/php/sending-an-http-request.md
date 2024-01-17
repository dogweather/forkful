---
title:                "Senden einer http-Anfrage"
html_title:           "PHP: Senden einer http-Anfrage"
simple_title:         "Senden einer http-Anfrage"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Was & Warum?
Das Senden einer HTTP-Anfrage ist ein zentraler Bestandteil der Programmierung in PHP. Es ermöglicht es Programmierern, Daten von anderen Servern abzurufen oder mit APIs zu interagieren. Dies ist besonders nützlich, um dynamische Inhalte auf Websites zu generieren.

# So geht's:
Um eine HTTP-Anfrage in PHP zu senden, verwenden wir die Funktion `file_get_contents()`. Wir geben dabei die URL der gewünschten Webseite oder API an und speichern die Rückgabewerte in einer Variable. Beispiel:

```PHP
$response = file_get_contents('https://example.com/api/data');
echo $response; // Gibt die Daten als Text aus
```

Die `file_get_contents()` Funktion gibt standardmäßig die Daten als String zurück, aber wir können auch ein assoziatives Array erhalten, indem wir den Parameter `true` hinzufügen. Beispiel:

```PHP
$response = file_get_contents('https://example.com/api/data', true);
print_r($response); // Gibt die Daten als Array aus
```

# Tief eintauchen:
Das Senden von HTTP-Anfragen in PHP ist seit langem eine gängige Methode, um mit externen Datenquellen zu kommunizieren. Eine alternative Methode ist die Verwendung der cURL-Bibliothek, die jedoch etwas komplexer ist. Um eine HTTP-Anfrage mit cURL zu senden, müssen wir eine Ressource erstellen, die Einstellungen festlegen und die Anfrage ausführen. Zum Beispiel:

```PHP
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, 'https://example.com/api/data');
$response = curl_exec($ch);
curl_close($ch);
echo $response; // Gibt die Daten als Text aus
```

In der Regel wird die Verwendung von `file_get_contents()` jedoch bevorzugt, da sie einfacher zu verwenden ist und keine zusätzlichen Bibliotheken erfordert.

# Siehe auch:
- Offizielle PHP-Dokumentation zu [file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- Offizielle PHP-Dokumentation zu [cURL](https://www.php.net/manual/en/book.curl.php)