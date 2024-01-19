---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP to proces, w którym przeglądarka internetowa (lub inny klient HTTP) prosi o dane od serwera. Programiści robią to, aby pozyskać i manipulować danymi z internetu.

## Jak to zrobić:

Możemy użyć narzędzia `curl` do wysyłania żądań HTTP, takie jak GET, POST, DELETE, itp. Zobacz poniższy kod:

```Bash 
# Proste żądanie GET
curl http://example.com 

# Żądanie POST z danymi
curl -X POST -d "param1=value1&param2=value2" http://example.com
```
Odpowiedzi serwera otrzymamy w terminalu. 

## W głąb tematu:

Wysyłanie żądań HTTP jest częścią protokołu HTTP, który został zaprojektowany w 1989 roku przez Tima Bernersa-Lee'a. Alternatywą dla `curl` z bash jest `wget`, który ma podobną funkcjonalność. 

Szczegół implementacji: bash wywołuje `curl` jako proces dziecko, przesyła żądanie HTTP, a `curl` przetwarza odpowiedź i zwraca ją do bash.

## Zobacz też:

- Dokumentacja `curl`: https://curl.haxx.se/docs/manpage.html
- Dokumentacja `wget`: https://www.gnu.org/software/wget/manual/wget.html
- Poradnik HTTP dla programistów: https://developer.mozilla.org/pl/docs/Web/HTTP/Overview