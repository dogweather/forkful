---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysyłanie żądania HTTP to proste zapytanie do serwera, które można porównać do prośby o informacje. Programiści wykonują to, aby komunikować się z serwerami, pobierać dane, przesyłać dane itp.

## Jak To Zrobić:

PHP udostępnia kilka wbudowanych funkcji do obsługi żądań HTTP. Oto przykład użycia `file_get_contents` do wykonania zapytania GET:

```PHP
<?php
$response = file_get_contents('http://example.com');
echo $response;
?>
```

Odpowiedź serwera zostanie wyświetlona przy użyciu funkcji `echo`. Możesz również wysłać zapytania POST, takie jak:

```PHP
<?php
$options = array(
    'http' => array(
        'header'  => "Content-type: application/x-www-form-urlencoded\r\n",
        'method'  => 'POST',
        'content' => http_build_query(array('key' => 'value')),
    ),
);
$context = stream_context_create($options);
$response = file_get_contents('http://example.com', false, $context);
echo $response;
?>
```

## Deep Dive:

Choć `file_get_contents` jest prosta i popularna, istnieje wiele innych sposobów wysyłania zapytań HTTP w PHP. Biblioteka cURL jest jednym z popularnych wyborów ze względu na jej wszechstronność.

Warto zauważyć, że pierwotna wersja PHP nie obsługiwała żądań HTTP. Funkcje te zostały dodane w późniejszych wersjach, gdy stało się jasne, że PHP jest doskonałym narzędziem do tworzenia dynamicznych stron internetowych opartych na danych serwera.

## Zobacz Również:

1. [Dokumentacja PHP na temat `file_get_contents`](https://www.php.net/manual/pl/function.file-get-contents.php)
2. [Dokumentacja PHP na temat stream_context_create](https://www.php.net/manual/pl/function.stream-context-create.php)
3. [Dokumentacja PHP na temat cURL](https://www.php.net/manual/pl/book.curl.php)