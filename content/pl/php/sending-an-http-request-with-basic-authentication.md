---
title:                "PHP: Wysyłanie żądania http z podstawową uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawową uwierzytelnieniem"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Wiele aplikacji internetowych wymaga uwierzytelniania użytkowników, aby zapewnić bezpieczeństwo i prywatność danych. Wysyłanie zabezpieczonych żądań HTTP z podstawową autoryzacją jest jednym ze sposobów na weryfikację tożsamości użytkownika i uzyskanie dostępu do chronionych zasobów.

## Jak to zrobić

Aby wysłać żądanie HTTP z podstawową autoryzacją w PHP, należy najpierw użyć funkcji `curl_init ()`, która tworzy nową sesję cURL. Następnie ustalamy adres URL, do którego chcemy wysłać żądanie, za pomocą funkcji `curl_setopt ()`. Ustawiamy również opcje uwierzytelniania, używając `CURLOPT_HTTPAUTH` i `CURLOPT_USERPWD`.

Następnie tworzymy nagłówek z danymi uwierzytelniającymi, przy użyciu funkcji `curl_setopt` i ustawiamy go jako `CURLOPT_HTTPHEADER`. Następnie możemy wykonać żądanie, używając funkcji `curl_exec ()` i otrzymać odpowiedź w zmiennej.

Oto przykładowy kod:

```PHP
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, 'https://www.example.com/api/users');
curl_setopt($ch, CURLOPT_HTTPAUTH, CURLAUTH_BASIC);
curl_setopt($ch, CURLOPT_USERPWD, "username:password");
curl_setopt($ch, CURLOPT_HTTPHEADER, array(
  'Content-Type: application/json',
  'Accept: application/json'
));
$output = curl_exec($ch);
```

W powyższym przykładzie wysyłamy żądanie GET do endpointa `/api/users`, używając danych uwierzytelniających `username:password`. Ustawiamy również nagłówki `Content-Type` i `Accept`, aby umożliwić komunikację w formacie JSON.

Po wykonaniu żądania możemy wyświetlić odpowiedź, używając zmiennej `$output`.

## Głębsza analiza

Dokumentacja PHP zawiera szczegółową informację na temat wysyłania żądań HTTP i obsługi uwierzytelniania. Możesz dowiedzieć się więcej o funkcji `curl_setopt` oraz opcjach `CURLOPT_HTTPAUTH` i `CURLOPT_USERPWD`. Ponadto, można przeczytać o innych rodzajach autoryzacji, takich jak żetony OAuth, które są bardziej bezpieczne niż podstawowa autoryzacja.

## Zobacz także

Sprawdź poniższe linki, aby dowiedzieć się więcej o wysyłaniu żądań HTTP z podstawową autoryzacją w PHP:

- [Dokumentacja PHP: Funkcja curl_setopt](https://www.php.net/manual/en/function.curl-setopt.php)
- [Dokumentacja PHP: Opcja CURLOPT_HTTPAUTH](https://www.php.net/manual/en/function.curl-setopt.php)
- [Dokumentacja PHP: Opcja CURLOPT_USERPWD](https://www.php.net/manual/en/function.curl-setopt.php)
- [Dokumentacja PHP: Wysyłanie zabezpieczonych żądań HTTP](https://www.php.net/manual/en/features.http-auth.php)