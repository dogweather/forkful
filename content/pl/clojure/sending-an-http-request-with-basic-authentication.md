---
title:                "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
html_title:           "Arduino: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Wysyłanie żądania HTTP z podstawowym uwierzytelnieniem to proces, w którym wysyłamy żądanie do serwera HTTP, które zawiera uwierzytelnienie. Programiści robią to w celu zabezpieczenia dostępu do zasobów serwera.

## Jak to zrobić:
Wyślijmy prosty żądanie HTTP z podstawowym uwierzytelnieniem za pomocą biblioteki clj-http, której używamy w Clojure. 

```Clojure
(require '[clj-http.client :as client])

(let [username "user"
      password "pass"]
  (client/get "http://example.com" {:basic-auth [username password]}))
```

Twój output powinien wyglądać mniej więcej tak:

```Clojure
{:status 200
 :headers {"date" "Wed, 13 Jul 2016 11:06:38 GMT"
              [...] }
 :body "/html/body\" [...]"}
```

Gdy zapytanie jest prawidłowe, kod stanu HTTP powinien wynosić 200.

## Pogłębiona analiza
Historia uwierzytelnienia HTTP sięga roku 1996, kiedy to został ono oficjalnie wprowadzone w specyfikacji HTTP 1.0. Alternatywą dla podstawowego uwierzytelnienia jest uwierzytelnienie Digest i uwierzytelnienie typu Bearer, które są bezpieczniejsze, ale też bardziej skomplikowane w implementacji. Głównym detalem implementacji w uwierzytelnieniu podstawowym jest to, że dane uwierzytelniane są wysyłane niezaszyfrowane (w formie Base64), co oznacza, że są narażone na przechwytywanie.

## Zobacz też
- Spójrz na [dokumentację clj-http](https://github.com/dakrone/clj-http) do dogłębnego zrozumienia, jak korzystać z tej biblioteki w Clojure.
- Przeczytaj [dokumenty uwierzytelnienia HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) na MDN dla pełniejszego obrazu uwierzytelnienia HTTP.
- Sprawdź nasz [przewodnik po uwierzytelnianiu Digest](link-to-digest-guide), jeśli chcesz dowiedzieć się więcej o bezpieczniejszych alternatywach.