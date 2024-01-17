---
title:                "Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem"
html_title:           "PHP: Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Wysyłanie żądania HTTP z podstawową autoryzacją jest jedną z najczęściej wykorzystywanych technik przez programistów, gdy chcą uzyskać dostęp do chronionych zasobów na serwerze. Dzięki temu można skutecznie uwierzytelnić użytkownika i pozwolić mu na dostęp do prywatnych danych lub wykonywanie określonych operacji na serwerze.

## Jak to zrobić?

```PHP
// Przykładowe żądanie HTTP z podstawową autoryzacją
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, 'http://example.com/protected-resource');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, 'username:password');

$response = curl_exec($ch);

// Wyświetlenie odpowiedzi
echo $response;
```

W powyższym przykładzie używamy funkcji `curl_setopt` do ustawienia opcji żądania. Przede wszystkim, używamy `CURLOPT_HTTPAUTH` i ustawiamy go na `CURLAUTH_BASIC`, aby włączyć autoryzację podstawową. Następnie, używamy `CURLOPT_USERPWD` do przekazania loginu i hasła w formacie `username:password`. W odpowiedzi otrzymamy chroniony zasób z serwera, który możemy wyświetlić przy użyciu funkcji `echo`.

## Głębszy zanurzenie

Wysyłanie żądań HTTP z podstawową autoryzacją jest możliwe dzięki wbudowanej funkcji `curl` w PHP. Jest to popularne rozwiązanie, gdyż nie ma potrzeby instalacji dodatkowych bibliotek lub zewnętrznych modułów. Jednak istnieją także inne metody implementacji autoryzacji podstawowej, takie jak wykorzystanie nagłówka `Authorization` lub użycie klasy `HttpBasicAuth`z frameworka Symfony.

## Zobacz także

- [Dokumentacja PHP - curl_setopt](https://www.php.net/manual/en/function.curl-setopt.php)
- [Dokumentacja PHP - CURLAUTH_BASIC](https://www.php.net/manual/en/curl.constants.php)
- [Podstawowa autoryzacja - W3C](https://tools.ietf.org/html/rfc7235#section-2.3)