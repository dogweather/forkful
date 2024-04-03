---
date: 2024-01-20 18:00:12.160471-07:00
description: "How to (Jak to zrobi\u0107): W PHP do wysy\u0142ania \u017C\u0105da\u0144\
  \ HTTP u\u017Cywamy g\u0142\xF3wnie funkcji `file_get_contents` lub rozszerzenia\
  \ `cURL`. Oto przyk\u0142ady obu."
lastmod: '2024-03-13T22:44:35.493997-06:00'
model: gpt-4-1106-preview
summary: "W PHP do wysy\u0142ania \u017C\u0105da\u0144 HTTP u\u017Cywamy g\u0142\xF3\
  wnie funkcji `file_get_contents` lub rozszerzenia `cURL`."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

## How to (Jak to zrobić):
W PHP do wysyłania żądań HTTP używamy głównie funkcji `file_get_contents` lub rozszerzenia `cURL`. Oto przykłady obu:

```php
// Proste żądanie GET przy użyciu file_get_contents
$response = file_get_contents('http://example.com/api/data');
echo $response;

// Wykorzystanie cURL do żądania GET
$curl = curl_init('http://example.com/api/data');
curl_setopt($curl, CURLOPT_RETURNTRANSFER, true);
$response = curl_exec($curl);
curl_close($curl);
echo $response;
```

Sample output (Przykładowe wyjście):
```
{"name": "Jan Kowalski", "email": "jan@example.com"}
```

## Deep Dive (Dogłębna analiza):
`file_get_contents` jest prostą funkcją do szybkiego przesłania prostej treści, ale ma ograniczone możliwości. `cURL`, czyli Client URL Library, pozwala na bardziej szczegółową konfigurację żądań HTTP i obsługuje niemal wszystkie protokoły sieciowe.

Historia: `cURL` pojawił się w 1997, a jego autor, Daniel Stenberg, do dziś wprowadza aktualizacje. `file_get_contents` funkcjonuje od PHP 4.3.0 i jest znane z łatwości użycia.

Alternatywy: Nowoczesne alternatywy to Guzzle, Requests library czy Symfony's HttpClient, które oferują API oparte na obiektach i są bardziej elastyczne.

Szczegóły implementacyjne: Warto użyć bibliotek trzecich, gdy trzeba zarządzać wieloma żądaniami, autentykacją, cache'owaniem odpowiedzi itp., `cURL` daje możliwości, ale to biblioteki poziom wyżej zdają egzamin przy bardziej skomplikowanych zastosowaniach.

## See Also (Zobacz również):
- Oficjalna dokumentacja PHP na temat `file_get_contents`:
  https://www.php.net/manual/pl/function.file-get-contents.php
- Dokumentacja `cURL` w PHP:
  https://www.php.net/manual/pl/book.curl.php
- Guzzle, a PHP HTTP client:
  http://docs.guzzlephp.org/
- Requests for PHP:
  http://requests.ryanmccue.info/
- Symfony HttpClient component:
  https://symfony.com/doc/current/components/http_client.html
