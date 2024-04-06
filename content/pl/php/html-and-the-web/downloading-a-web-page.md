---
date: 2024-01-20 17:44:28.395010-07:00
description: "How to: (Jak to zrobi\u0107:) U\u017Cyjmy PHP i cURL, aby \u015Bci\u0105\
  gn\u0105\u0107 zawarto\u015B\u0107 strony. Oto przyk\u0142adowy kod."
lastmod: '2024-04-05T21:53:36.926955-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) U\u017Cyjmy PHP i cURL, aby \u015Bci\u0105gn\u0105\
  \u0107 zawarto\u015B\u0107 strony."
title: Pobieranie strony internetowej
weight: 42
---

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
