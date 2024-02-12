---
title:                "Wysyłanie żądania HTTP"
aliases:
- /pl/bash/sending-an-http-request.md
date:                  2024-01-20T17:59:12.897844-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
(Co i dlaczego?)

Wysyłanie zapytania HTTP to komunikacja między twoim komputerem a serwerem; mówiąc prościej, to tak, jakbyś wysłał list z prośbą do internetowego skrzynka pocztowego. Programiści robią to, by pobierać dane, wysyłać formularze lub interakcje z API sieciowymi - to jak podstawowe narzędzie w cyfrowej skrzynce z narzędziami.

## How to:
(Jak to zrobić:)

Użycie `curl` w Bashu to najprostsza forma wysyłania zapytania HTTP. Oto jak to zrobić:

```Bash
# Pobranie zawartości strony www
curl http://example.com

# Przykładowe wyjście, zawartość strony example.com
<!doctype html>
<html>
...
</html>

# Wysyłka danych POST
curl -d "login=jan&password=haslo123" -X POST http://example.com/login

# Przykładowe wyjście, odpowiedź serwera po wysłaniu danych POST
{"status": "ok", "message": "Successfully logged in!"}
```

## Deep Dive:
(Pogłębiona wiedza:)

`curl`, używane od 1997 roku, to narzędzie konsolowe do transferu danych z lub na serwer. Oprócz HTTP obsługuje wiele innych protokołów jak FTP czy SMTP. Istnieją alternatywy, np. `wget` (prostszy lecz ograniczony do pobierania plików) czy nowoczesne narzędzie jak `httpie` z lepszą czytelnością wyników.

Detale implementacyjne? `curl` może być używane w skryptach bashowych do automatyzacji procesów sieciowych, np. sprawdzania stanu serwera czy automatycznego pobierania danych. Uwaga: warto używać opcji `-m` do ustawienia limitu czasowego, by uniknąć zawiechy skryptu, gdy serwer nie odpowiada.

## See Also:
(Zobacz również:)

- Dokumentacja curl: https://curl.se/docs/
- Porównanie curl i wget: https://www.keycdn.com/blog/curl-vs-wget
- Httpie – alternatywa dla curl: https://httpie.io/
