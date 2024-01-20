---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i Dlaczego? 

Wysyłanie żądania HTTP z podstawowym uwierzytelnianiem to proces, w którym klient komunikuje się z serwerem, przekazując dane uwierzytelniające (z reguły login i hasło) w nagłówku żądania. Programiści robią to, aby uzyskać dostęp do zasobów serwera, które są chronione i wymagają uwierzytelniania.

## Jak to zrobić:

Aby wysłać żądanie HTTP z podstawowym uwierzytelnianiem w PHP, możesz skorzystać z biblioteki cURL. Poniżej znajduje się przykładowy kod:

```PHP
$ch = curl_init();

curl_setopt($ch, CURLOPT_URL, 'https://twojastrona.pl/zasoby');
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, 'login:haslo');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);

$response = curl_exec($ch);

curl_close($ch);

echo $response;
```

Ten skrypt wysyła żądanie GET do 'https://twojastrona.pl/zasoby' z podstawowym uwierzytelnianiem, używając loginu 'login' i hasła 'haslo'. Wynik żądania jest drukowany na standardowym wyjściu.

## Głębsze spojrzenie:

Podstawowe uwierzytelnianie HTTP to metoda uwierzytelniania, której używano w internetowej komunikacji z serwerem już od początku lat 90. Udostępnia ona podstawowe zabezpieczenia, ale nie jest zalecana w przypadku bardzo wrażliwych danych, ponieważ hasło jest przesyłane jako dane zakodowane w Base64, co jest dość łatwe do odczytania. 

Alternatywą dla podstawowego uwierzytelniania jest uwierzytelnianie znaku, uwierzytelnianie OAuth albo używanie tokenów JWT. Te metody są zazwyczaj bezpieczniejsze i skuteczniejsze.

Szczegóły implementacji zależą od konkretnego przypadku. W wybranych sytuacjach możesz potrzebować dostosować czas wygaśnięcia żądania, obsłużyć różne kody odpowiedzi, lub zaimplementować obsługę błędów.

## Zobacz również:

Tutaj znajdziesz więcej informacji na ten temat:

1. Autorytet informacji na temat PHP: [PHP.net](https://www.php.net/manual/en/book.curl.php)
2. Pewne zalecenia na temat autoryzacji HTTP: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
3. Jak zabezpieczyć dane uwierzytelniające przy przesyłaniu: [OWASP](https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html)