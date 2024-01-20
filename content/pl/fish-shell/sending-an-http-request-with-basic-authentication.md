---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP z podstawową autoryzacją oznacza przesyłanie danych wprowadzone przez użytkownika (nazwa użytkownika i hasło) w celu weryfikacji dostępu do zasobów stron internetowych. Programiści korzystają z niego, gdy chcą zabezpieczyć swoje strony webowe przed nieautoryzowanym dostępem.

## Jak to zrobić:

```Fish Shell
function http_get_auth
    set user "your_username"
    set password "your_password"
    set base64_auth (echo -n "$user:$password" | base64)
    set url "http://your_url.com"
    curl -H "Authorization: Basic $base64_auth" $url
end
```
Wywołaj tę funkcję, a otrzymasz odpowiedź HTTP od Twojego URLa.

## Pogłębianie wiedzy

1) Kontekst historyczny : Po raz pierwszy zaimplementowano podstawową autoryzację HTTP w specyfikacji HTTP 1.0 w 1996 roku. 
2) Alternatywy: Chociaż podstawowa autoryzacja jest prosta w użyciu, nie jest zbyt bezpieczna i często zastępuje się ją technikami autoryzacji bardziej zaawansowanymi, takimi jak autoryzacja Digest lub OAuth.
3) Detale implementacyjne: Podstawowa autoryzacja polega na wysłaniu nazwy użytkownika i hasła jako nieszyfrowanej, zakodowanej w Base64 ciągu znaków. Z tego powodu, zawsze powinna być używana w połączeniu z HTTPS.

## Zobacz także

1) [Podstawowa autoryzacja HTTP na Wikipedii](https://pl.wikipedia.org/wiki/Podstawowa_autoryzacja_HTTP)
2) [Przewodnik po autoryzacji HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP/Authentication)
3) [Więcej o Fish Shell](https://fishshell.com/docs/current/index.html)