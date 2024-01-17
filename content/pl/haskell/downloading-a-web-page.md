---
title:                "Pobieranie strony internetowej"
html_title:           "Haskell: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie stron internetowych jest procesem pozyskiwania zawartości strony z internetu i zapisywania jej na naszym urządzeniu. Programiści wykonują to dla różnych celów, na przykład do analizowania danych lub tworzenia aplikacji internetowych.

## Jak to zrobić:
Aby pobrać stronę internetową w Haskell, możesz użyć modułu [```Network.HTTP```](https://hackage.haskell.org/package/HTTP). Poniżej znajduje się przykładowy kod, który pobiera zawartość strony i wyświetla ją w konsoli.

```Haskell
import Network.HTTP

main = do
    response <- simpleHTTP $ getRequest "https://www.example.com"
    body <- getResponseBody response
    putStrLn body
```

Otrzymany wynik będzie wyglądał podobnie do poniższego:

```html
<!DOCTYPE html>
<html>
<head>
  <title>Example Domain</title>
  <meta charset="UTF-8">
  <meta http-equiv="Content-type" content="text/html; charset=UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
</head>
<body>
  <div>
      <h1>Example Domain</h1>
      <p>This domain is for use in illustrative examples in documents. You may use this
    domain in literature without prior coordination or asking for permission.</p>
  </div>
</body>
</html>
```

## Głębsze spojrzenie:
Pobieranie stron internetowych było częścią programowania od samego początku. W większości języków programowania jest to zadanie dość prosty, ale w Haskell możemy wykorzystać moduł ```Network.HTTP``` do bardziej zaawansowanych zadań, takich jak pobieranie stron z wykorzystaniem protokołu ```https```.

Alternatywą dla modułu ```Network.HTTP``` jest [```HTTP Conduit```](https://hackage.haskell.org/package/http-conduit), który zapewnia bardziej elastyczny sposób na pobieranie stron internetowych.

Szczegóły implementacyjne pobierania stron internetowych są dość złożone i wykraczają poza zakres tego artykułu. Jeśli jesteś zainteresowany, możesz zajrzeć do dokumentacji modułu ```Network.HTTP```.

## Zobacz też:
- [Dokumentacja modułu ```Network.HTTP```](https://hackage.haskell.org/package/HTTP/docs/Network-HTTP.html)
- [Moduł ```HTTP Conduit```](https://hackage.haskell.org/package/http-conduit)
- [Poradnik od HaskellWiki na temat pobierania stron](https://wiki.haskell.org/Download_web_page)