---
title:                "Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem"
html_title:           "PHP: Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem"
simple_title:         "Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem jest często wykorzystywane w aplikacjach internetowych do uwierzytelniania użytkowników. Dzięki temu możliwe jest dostarczenie bezpiecznego dostępu do chronionych zasobów.

## Jak to zrobić

```PHP
<?php
    // Przykład wysyłania żądania HTTP z podstawowym uwierzytelnianiem
    $username = "admin";
    $password = "password";

    // Utworzenie nagłówka z danymi uwierzytelniającymi
    $auth = base64_encode($username . ":" . $password);
    $headers = array('Authorization: Basic ' . $auth);

    // Wywołanie żądania HTTP
    $ch = curl_init();
    curl_setopt($ch, CURLOPT_URL, 'https://example.com/protected-resource');
    curl_setopt($ch, CURLOPT_HTTPHEADER, $headers);
    curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
    $output = curl_exec($ch);
    curl_close($ch);

    // Wyświetlenie odpowiedzi serwera
    echo $output;
?>
```

Przykładowy wynik:

```
<HTTP 200 OK>
```

## Gleboki zanurzenia

Podstawowe uwierzytelnianie jest metodą uwierzytelniania, w której dane uwierzytelniające są przesyłane w postaci nieszyfrowanej w nagłówku żądania HTTP. Jest to metoda bezpieczeństwa, którą należy stosować z ostrożnością, ponieważ dane uwierzytelniające są przechowywane w formie zdekodowanej na serwerze i mogą być łatwo przechwycone przez nieuprawnionych użytkowników.

## Zobacz także

- [Dokumentacja PHP o funkcji curl_setopt()](https://www.php.net/manual/en/function.curl-setopt.php)
- [Poradnik dla początkujących: Wysyłanie żądania HTTP przy użyciu PHP i cURL](https://code.tutsplus.com/tutorials/php-getting-started-with-curl--net-1431)
- [RFC 2617: HTTP Basic Authentication](https://tools.ietf.org/html/rfc2617)