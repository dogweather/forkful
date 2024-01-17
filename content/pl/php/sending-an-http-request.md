---
title:                "Wysyłanie żądania http"
html_title:           "PHP: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądań HTTP jest niezbędnym elementem procesu tworzenia stron internetowych. Służy ono do pobierania danych z serwera i wyświetlania ich na stronie lub do przekazywania danych z formularzy do serwera. Programiści często korzystają z tego narzędzia, aby zadbać o płynne i bezproblemowe działanie stron internetowych.

## Jak:
Oto przykładowy kod PHP, który wysyła żądanie HTTP i wyświetla otrzymaną odpowiedź:
```PHP
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, "https://example.com");
curl_exec($ch);
curl_close($ch);

// Output:
Welcome to Example.com!
```
W powyższym przykładzie użyto funkcji `curl_init()` do utworzenia obsługi żądania HTTP. Następnie, przy użyciu `curl_setopt()`, ustawiono adres URL, do którego ma zostać wysłane żądanie. W kolejnym wierszu użyto funkcji `curl_exec()`, która wysyła żądanie i zwraca odpowiedź. Na koniec, przy użyciu `curl_close()`, zamknięto zasoby połączenia. 

Warto pamiętać, że funkcja `curl_setopt()` pozwala też na ustawianie innych opcji, takich jak metoda żądania, dane do wysłania czy nagłówki żądania. Możliwości jest wiele, więc warto zapoznać się z dokumentacją funkcji `curl_setopt()` lub zajrzeć do sekcji "Zobacz też".

## Deep Dive:
Wysyłanie żądań HTTP jest jednym z wielu sposobów komunikacji między klientem a serwerem. Alternatywami do funkcji `curl_exec()` są np. funkcje `file_get_contents()`, `fopen()` czy `fsockopen()`. Każda z tych funkcji ma swoje zalety i wady, ale `curl_exec()` jest najczęściej wybierane ze względu na swoją elastyczność i dużą liczbę dostępnych opcji. 

Funkcja `curl_exec()` korzysta z biblioteki cURL, która powstała w 1997 roku i jest ciągle rozwijana. Jest ona wysoce wydajna i obsługuje wiele protokołów, takich jak HTTP, HTTPS, FTP czy FTPS. Dzięki niej możemy nie tylko wysyłać żądania, ale także pobierać zawartość stron internetowych, przesyłać pliki czy ustawiać nagłówki. 

## Zobacz też:
- [Dokumentacja funkcji `curl_setopt()` w języku polskim](https://www.php.net/manual/pl/function.curl-setopt.php)
- [Oficjalna dokumentacja biblioteki cURL](https://curl.haxx.se/libcurl/)
- [Porównanie funkcji do wysyłania żądań HTTP w PHP](https://www.php.net/manual/en/function.file-get-contents.php)