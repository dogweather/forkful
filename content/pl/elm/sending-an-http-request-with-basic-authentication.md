---
title:                "Elm: Wysyłanie żądania http z podstawowym uwierzytelnieniem"
simple_title:         "Wysyłanie żądania http z podstawowym uwierzytelnieniem"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Dlaczego chcielibyśmy użyć autoryzacji podstawowej przy wysyłaniu żądania HTTP? Ponieważ jest to wygodny sposób na uwierzytelnienie naszych żądań do zabezpieczonych stron internetowych lub serwerów API. W tym wpisie pokażemy, jak dokładnie tego dokonać w języku Elm.

## Jak to zrobić

Kodowanie przy użyciu języka Elm jest proste i przyjemne. Aby wysłać żądanie HTTP z autoryzacją podstawową, wystarczy skorzystać z modułu "Http" oraz funkcji "send" i "basicAuth". Poniżej znajduje się przykładowy kod wraz z oczekiwanym wynikiem.

```Elm
import Http
import Basics exposing (..)

sendBasicAuthRequest : String -> String -> String -> Cmd Msg
sendBasicAuthRequest username password url =
  Http.send BasicAuth
    { method = "GET"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , withCredentials = False
    , username = Just username
    , password = Just password
    }
```

Wynik:

```Elm
sendBasicAuthRequest "username" "password" "https://example.com/api" = HTTP/1.1 200 OK
```

## Deep Dive

Podstawowa autoryzacja w protokole HTTP jest procesem, w którym klient wysyła swoje dane logowania w nagłówku żądana "Authorization". Serwer odczytuje te dane i weryfikuje, czy są one poprawne. W języku Elm możemy zaimplementować to za pomocą modułu "Http" i funkcji "basicAuth". Należy jednak pamiętać, że autoryzacja podstawowa jest niezalecana ze względu na swoją słabą ochronę danych.

## Zobacz także

* [Dokumentacja języka Elm](https://guide.elm-lang.org/)
* [Informacje o protokole HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
* [Wprowadzenie do autoryzacji HTTP](https://www.digitalocean.com/community/tutorials/http-basic-authentication-in-node-js)