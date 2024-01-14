---
title:                "Clojure: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach wiele aplikacji i serwisów korzysta z protokołu HTTP do komunikacji miedzy klientami i serwerami. Aby zapewnić bezpieczną wymianę danych, niektóre z tych żądań muszą być uwierzytelnione. W tym artykule dowiesz się, dlaczego warto używać autoryzacji podstawowej w żądaniach HTTP przy użyciu języka programowania Clojure.

## Jak to zrobić

W celu przesłania żądania HTTP z autoryzacją podstawową, należy użyć funkcji `(clj-http.client/post "url" {:basic-auth ["username" "password"]})`. Kody odpowiedzi HTTP zostaną zwrócone w formacie mapy Clojure, co ułatwia dalsze przetwarzanie.

```Clojure
(require '[clj-http.client :as client])

(def response (client/post "https://example.com" {:basic-auth ["john" "passw0rd"]}))

(println (:status response)) ;; wyświetli kod odpowiedzi, np. 200

(println (:body response)) ;; wyświetli zawartość odpowiedzi, np. Hello World!
```

## Deep Dive

Podczas autoryzacji podstawowej, klient przesyła nazwę użytkownika i hasło w formacie zakodowanym Base64 w nagłówku `Authorization`. Serwer następnie uwierzytelnia te dane, a jeśli są one poprawne, zwraca kod odpowiedzi 200, co oznacza udane uwierzytelnienie. W przypadku nieprawidłowych danych, serwer zwraca kod odpowiedzi 401, informując o błędzie uwierzytelnienia.

Warto również pamiętać, że autoryzacja podstawowa nie jest bezpieczna, ponieważ nazwa użytkownika i hasło są wysyłane w formie tekstu jawnej. Zaleca się używanie autoryzacji za pomocą tokenów lub innego bezpiecznego sposobu uwierzytelniania w przypadku wymagania bezpieczeństwa.

## Zobacz także

Jeśli chcesz dowiedzieć się więcej na temat przesyłania żądań HTTP z uwierzytelnieniem podstawowym w Clojure, polecamy zapoznanie się z dokumentacją języka Clojure oraz narzędziem `clj-http`.

- Dokumentacja Clojure: https://clojure.org/
- Narzędzie clj-http: https://github.com/dakrone/clj-http