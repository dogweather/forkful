---
title:                "Wysyłanie zapytania http"
html_title:           "Clojure: Wysyłanie zapytania http"
simple_title:         "Wysyłanie zapytania http"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Odczytywanie i przesyłanie danych za pomocą protokołu HTTP jest niezbędnym elementem tworzenia aplikacji internetowych. Dzięki temu możemy korzystać z wielu usług dostępnych w sieci, np. wysyłania formularzy, pobierania informacji z serwerów lub interakcji z innymi stronami internetowymi.

## Jak to zrobić

```Clojure
;; Wysłanie żądania GET
(:require [clj-http.client :as client])

(client/get "https://www.example.com")
```

W ten sposób możemy wysłać proste żądanie GET do strony "www.example.com" i otrzymać odpowiedź w formacie JSON. Jako wynik otrzymamy zestaw danych opisujących nagłówki, treść i kod odpowiedzi.

```Clojure
;; Wysłanie żądania POST
(:require [clj-http.client :as client])

(client/post "https://www.example.com/login" {:form-params {:username "Jan" :password "haslo123"}})
```

W przypadku żądania POST możemy przekazać dodatkowe parametry w formacie {:form-params }. W powyższym przykładzie wysyłamy żądanie logowania z danymi użytkownika i otrzymujemy odpowiedź w postaci sesji użytkownika lub błędu.

## Głębszy zanurzanie się

W celu lepszego zrozumienia procesu wysyłania i odbierania żądań HTTP, warto poznać podstawy protokołu oraz dostępne opcje w bibliotece Clojure dla tego typu operacji.

Protokół HTTP dzielimy na dwa rodzaje żądań - GET i POST. W żądaniu GET przesyłamy jedynie nagłówki, a w POST możemy przesyłać również ciało żądania, np. w formie formularza. Odpowiedzią na każde żądanie jest zestaw danych, który jest interpretowany w zależności od tego, w jakim formacie został przesłany.

### Dodatkowe opcje w bibliotece Clojure

W bibliotece [clj-http](https://github.com/dakrone/clj-http) dostępne są dodatkowe opcje, które pozwalają na konfigurację żądania, np. ustawianie nagłówków, timeoutów czy trybu debugowania. Dzięki temu możemy dostosować nasze żądanie do indywidualnych potrzeb.

## Zobacz też

- [Dokumentacja Clojure HTTP Client](https://github.com/dakrone/clj-http)
- [Wysyłanie danych za pomocą HTTP](https://clojuredocs.org/clojure.core/slurp)
- [Podstawy protokołu HTTP](https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html)