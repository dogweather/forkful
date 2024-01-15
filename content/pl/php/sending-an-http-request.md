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

## Dlaczego

Bez względu na to, czy jesteś programistą czy początkującym w dziedzinie tworzenia stron internetowych, przychodzi moment, w którym musisz wysłać żądanie HTTP. Może to być część Twojej pracy lub po prostu sposób na sprawdzenie stanu swojej strony lub aplikacji. W każdym przypadku, wysyłanie żądania HTTP jest nieodłączną częścią pracy z aplikacjami webowymi.

## Jak to zrobić

Aby wysłać żądanie HTTP w PHP, wystarczy użyć funkcji `file_get_contents()` lub `curl_exec()`. Oto przykładowy kod:

```PHP
// Przykład użycia funkcji file_get_contents()
$response = file_get_contents('https://example.com/api');
echo $response; // Wyświetli zawartość zwróconą przez API

// Przykład użycia funkcji curl_exec()
$ch = curl_init();
curl_setopt($ch, CURLOPT_URL, 'https://example.com/api');
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1);
$response = curl_exec($ch);
echo $response; // Wyświetli zawartość zwróconą przez API
curl_close($ch);
```

Pierwszy przykład używa funkcji `file_get_contents()` do pobrania zawartości z podanego adresu URL. Drugi przykład wykorzystuje funkcje `curl_init()`, `curl_setopt()` i `curl_exec()` do wysłania bardziej zaawansowanego żądania HTTP, z możliwością ustawienia dodatkowych opcji takich jak nagłówki czy autoryzacja.

## Deep Dive

Aby lepiej zrozumieć proces wysyłania żądań HTTP w PHP i dostosować go do swoich potrzeb, warto poznać kilka dodatkowych informacji.

### Typy żądań HTTP

Wysyłane żądania mogą być jednym z trzech typów: GET, POST lub HEAD. GET jest domyślnym typem żądania i służy do pobierania danych z serwera. POST jest wykorzystywany do przesyłania i przetwarzania danych, na przykład formularzy na stronie. HEAD jest podobny do GET, ale zwraca tylko nagłówki bez ciała odpowiedzi.

### Nagłówki HTTP

Nagłówki to dane, które są przesyłane wraz z żądaniem i pomagają w jego obsłudze. Istnieje wiele różnych nagłówków, ale te najczęściej używane przy wysyłaniu żądań HTTP w PHP to "Content-Type", "Content-Length" i "Authorization". Możesz je ustawić za pomocą funkcji `curl_setopt()` lub jako parametr w funkcji `file_get_contents()`.

### Obsługa odpowiedzi HTTP

Po wysłaniu żądania, serwer zwraca odpowiedź, która zawiera informacje o stanie żądania oraz ewentualne dane. Kod stanu żądania jest zwykle widoczny jako pierwsze trzy cyfry w nagłówku odpowiedzi. Przykładowo, kod 200 oznacza sukces, a 404 oznacza że strona nie została znaleziona. W zależności od kodu stanu, możesz odpowiednio przetworzyć odpowiedź w swoim kodzie.

## Zobacz także

- [Dokumentacja PHP: Wprowadzenie do funkcji HTTP](https://www.php.net/manual/en/intro.http.php)
- [Dokumentacja PHP: Function file_get_contents()](https://www.php.net/manual/en/function.file-get-contents.php)
- [Dokumentacja PHP: CURL - Obsługa transferu URL](https://www.php.net/manual/en/book.curl.php)