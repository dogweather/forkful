---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Clojure: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP z podstawową autoryzacją jest sposobem na uwierzytelnienie się do serwera za pomocą nazwy użytkownika i hasła. Jest to powszechne w programowaniu, ponieważ pozwala na ochronę danych i zasobów przed niepowołanym dostępem.

## Jak to zrobić:
```Clojure
(require '[clj-http.client :as http])

(http/get "https://example.com" :basic-auth "username" "password")
```

W powyższym przykładzie wykorzystujemy bibliotekę clj-http, aby wysłać żądanie GET do witryny "example.com". Dodatkowo przekazujemy również nazwę użytkownika i hasło jako parametry podstawowej autentykacji.

## Deep Dive:
Podstawowa autoryzacja została pierwotnie wprowadzona w protokole HTTP w 1996 roku. Dzięki temu programiści mogą szybko i łatwo dodawać funkcję uwierzytelniania do swoich aplikacji internetowych. Alternatywnym podejściem jest wykorzystanie bardziej bezpiecznej autoryzacji, taka jak OAuth.

Biblioteka clj-http jest popularnym narzędziem do wysyłania żądań HTTP w języku Clojure. Jest ona zależna od biblioteki Apache HttpComponents, która obsługuje różne typy autoryzacji, w tym podstawową.

## Zobacz również:
- [Dokumentacja biblioteki clj-http](https://github.com/dakrone/clj-http)
- [Oficjalna specyfikacja protokołu HTTP](https://tools.ietf.org/html/rfc2616)
- [Porównanie metod uwierzytelniania w protokole HTTP](https://www.httpwatch.com/httpgallery/authentication/)