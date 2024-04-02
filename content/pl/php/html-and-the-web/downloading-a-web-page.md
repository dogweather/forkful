---
date: 2024-01-20 17:44:28.395010-07:00
description: "(Po co i dlaczego?) Pobieranie strony internetowej to proces \u015B\
  ci\u0105gania jej zawarto\u015Bci, by przetworzy\u0107 je lokalnie. Programi\u015B\
  ci robi\u0105 to, aby odczyta\u0107\u2026"
lastmod: '2024-03-13T22:44:35.495987-06:00'
model: gpt-4-1106-preview
summary: "(Po co i dlaczego?) Pobieranie strony internetowej to proces \u015Bci\u0105\
  gania jej zawarto\u015Bci, by przetworzy\u0107 je lokalnie. Programi\u015Bci robi\u0105\
  \ to, aby odczyta\u0107\u2026"
title: Pobieranie strony internetowej
weight: 42
---

## What & Why?
(Po co i dlaczego?)

Pobieranie strony internetowej to proces ściągania jej zawartości, by przetworzyć je lokalnie. Programiści robią to, aby odczytać dane, zautomatyzować testy lub monitorować zmiany treści.

## How to:
(Jak to zrobić:)

Użyjmy PHP i cURL, aby ściągnąć zawartość strony. Oto przykładowy kod:

```PHP
<?php
$url = "http://example.com";
$ch = curl_init($url);
curl_setopt($ch, CURLOPT_RETURNTRANSFER, true);
$webpage = curl_exec($ch);
if($webpage === false) {
    echo "Nie udało się pobrać strony: " . curl_error($ch);
} else {
    echo "Zawartość strony:\n$webpage";
}
curl_close($ch);
?>
```

Gdy uruchomisz, spodziewaj się wyjścia podobnego do:

```
Zawartość strony:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</html>
```

## Deep Dive:
(Głębsze spojrzenie:)

Pobieranie stron internetowych to nic nowego — robią to przeglądarki, gdy je przeglądamy. PHP implementuje to za pomocą biblioteki cURL lub alternatywnie funkcji `file_get_contents()`, jeśli nie potrzebujemy skomplikowanych opcji. Implementacja cURL w PHP umożliwia bardziej zaawansowane operacje, takie jak obsługa ciasteczek, przekazywanie nagłówków czy autoryzacja HTTP. Historia funkcji cURL sięga 1997 roku, a jej stabilność i elastyczność sprawiają, że jest do dziś szeroko stosowana mimo pojawienia się nowych bibliotek, jak Guzzle w PHP.

## See Also:
(Zobacz również:)

- [PHP cURL](https://www.php.net/manual/en/book.curl.php) - oficjalna dokumentacja PHP cURL.
- [Guzzle](http://docs.guzzlephp.org/en/stable/) - współczesna biblioteka do operacji HTTP w PHP.
- [HTTP Requests with cURL](https://curl.se/) – strona główna projektu cURL, z dokumentacją do pobrania strony.
