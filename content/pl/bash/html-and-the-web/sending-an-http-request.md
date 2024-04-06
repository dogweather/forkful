---
date: 2024-01-20 17:59:12.897844-07:00
description: "How to: (Jak to zrobi\u0107:) U\u017Cycie `curl` w Bashu to najprostsza\
  \ forma wysy\u0142ania zapytania HTTP. Oto jak to zrobi\u0107."
lastmod: '2024-04-05T21:53:37.009019-06:00'
model: gpt-4-1106-preview
summary: "(Jak to zrobi\u0107:) U\u017Cycie `curl` w Bashu to najprostsza forma wysy\u0142\
  ania zapytania HTTP."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

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
