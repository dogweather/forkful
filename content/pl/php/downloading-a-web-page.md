---
title:                "Pobieranie strony internetowej"
html_title:           "PHP: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/php/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie strony internetowej jest nieodzownym elementem programowania w PHP, szczególnie jeśli chodzi o tworzenie skryptów automatyzujących pobieranie plików lub analizowanie danych z innych stron. Jest to szybki i skuteczny sposób na uzyskanie potrzebnych informacji.

## Jak to zrobić

```PHP
$url = 'https://www.example.com'; //adres URL strony do pobrania

//inicjalizacja sesji CURL
$ch = curl_init();

//ustawienie opcji pobierania strony
curl_setopt($ch, CURLOPT_URL, $url); //ustawienie adresu URL
curl_setopt($ch, CURLOPT_RETURNTRANSFER, 1); //zwrócenie pobranej strony jako wyniku zamiast wyświetlenia
curl_setopt($ch, CURLOPT_FOLLOWLOCATION, 1); //umożliwienie obsługi przekierowań na stronie
curl_setopt($ch, CURLOPT_MAXREDIRS, 5); //maksymalna liczba przekierowań
curl_setopt($ch, CURLOPT_SSL_VERIFYPEER, 0); //ignorowanie certyfikatów SSL

//wykonanie zapytania i zapisanie wyniku do zmiennej
$result = curl_exec($ch);

//sprawdzenie czy wystąpił błąd
if (curl_errno($ch)){
  echo 'Błąd pobierania strony: ' . curl_error($ch);
}

//zamknięcie sesji CURL
curl_close($ch); 

//wyświetlenie pobranej strony
echo $result;
```

### Output
```
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title> //tytuł strony
  <meta charset="UTF-8"> //kodowanie znaków
  <meta name="viewport" content="width=device-width, initial-scale=1.0"> //ustawienia dla urządzeń mobilnych
</head>
<body>
  <h1>Example Domain</h1> //tytuł strony
  <p>This domain is for use in illustrative examples in documents. You may use this
  domain in literature without prior coordination or asking for permission.</p> //tekst na stronie
</body>
</html>
```

## Deep Dive

Pobieranie strony przy użyciu PHP odbywa się za pomocą biblioteki CURL, która jest częścią standardowej instalacji PHP. Biblioteka ta pozwala na ustawienie różnych opcji, takich jak obsługa przekierowań czy ignorowanie certyfikatów SSL. Dzięki temu można dostosować pobieranie do swoich potrzeb.

## Zobacz również

- Dokumentacja CURL w PHP: https://www.php.net/manual/en/book.curl.php
- Przydatne informacje o pobieraniu stron w PHP: https://www.codementor.io/@ganesh85/how-to-scrape-a-website-using-php-du108266t