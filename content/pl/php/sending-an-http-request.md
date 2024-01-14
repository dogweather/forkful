---
title:                "PHP: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz pracować z siecią internetową lub korzystać z zasobów internetowych, wysyłanie zapytań HTTP jest nieodzownym elementem. Dzięki temu prostemu procesowi możesz pobierać dane z innych stron internetowych, przesyłać formularze, a nawet tworzyć własne serwisy internetowe. W tym artykule dowiecie się, jak wysyłać zapytania HTTP za pomocą języka PHP.

## Jak to zrobić

Wysłanie zapytania HTTP za pomocą PHP jest bardzo proste, wystarczy użyć funkcji `file_get_contents ()`. Poniżej znajduje się przykładowy kod, który pobiera dane z zewnętrznego serwera i wyświetla je na stronie:

```PHP
<?php
    $url = 'https://www.example.com/';
    $data = file_get_contents($url);
    echo $data;
?>
```

Po wykonaniu tego kodu, na stronie pojawi się zawartość strony internetowej pod adresem `www.example.com`. Możesz również użyć tej funkcji do pobrania zawartości innych zasobów, takich jak obrazy czy filmy.

Inną opcją jest użycie funkcji `curl_exec ()`, która oferuje większą kontrolę nad sposobem wysyłania zapytań HTTP. Poniżej znajduje się przykład użycia tej funkcji, który wyświetla odpowiedź serwera w formacie JSON:

```PHP
<?php
    $url = 'https://api.example.com/users/1';
    $ch = curl_init($url);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
    $data = curl_exec($ch);
    curl_close($ch);
    $response = json_decode($data, true);
    echo "User ID: " . $response['id'];
    echo "User Name: " . $response['name'];
    echo "User Email: " . $response['email'];
?>
```

Ten kod pobiera dane użytkownika o ID równym 1 z wybranego API i wyświetla je na stronie.

## Głębszy zanurzenie

Wysyłanie zapytań HTTP jest integralną częścią programowania w języku PHP. Dzięki temu możesz komunikować się z innymi zasobami sieciowymi i korzystać z ich danych. Istnieje wiele funkcji i narzędzi, które można wykorzystać do wysyłania i odbierania zapytań HTTP w PHP. Warto dokładniej przyjrzeć się dokumentacji PHP, aby dowiedzieć się, jak wykorzystać te narzędzia w swoich projektach.

## Zobacz także

- Dokumentacja PHP: https://www.php.net/manual/pl/index.php
- Przewodnik po wysyłaniu zapytań HTTP w PHP: https://www.sitepoint.com/guide-php-pseudo-variables/
- Przykłady wykorzystania funkcji cURL w PHP: https://www.php.net/manual/pl/book.curl.php