---
title:                "Clojure: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Dlaczego pobieranie strony internetowej jest ważne?

Pobieranie stron internetowych może być bardzo użyteczne w wielu sytuacjach. Możesz używać go do pobierania danych do analizy, automatycznego zapisu informacji z różnych stron lub nawet do stworzenia własnego bot-a internetowego. Możliwości są nieograniczone!

# Jak to zrobić?

Pobieranie strony internetowej w języku Clojure jest bardzo proste. Wystarczy użyć jednej z wielu dostępnych bibliotek, takich jak `clj-http` lub `http-kit`, które ułatwią ten proces.

```Clojure
(require '[clj-http.client :as http])

(def page (http/get "https://www.example.com"))

(println (:body page)) ;; wyświetla treść pobranej strony
(println (:status page)) ;; wyświetla status odpowiedzi (np. 200 oznacza sukces)
```

Ten kod używa biblioteki `clj-http`, aby pobrać stronę z adresu URL i wyświetlić jej treść oraz status odpowiedzi. Możesz również dodać dodatkowe parametry, takie jak nagłówki czy dane formularza, aby dostosować zapytanie.

# Głębsza analiza

Pobieranie strony internetowej może być również użyteczne do bardziej zaawansowanych zastosowań. Na przykład, możesz wykorzystać możliwości języka Clojure, aby przeanalizować zebrane dane lub użyć jej w połączeniu z innymi bibliotekami do tworzenia silniejszych narzędzi.

Na przykład, w połączeniu z biblioteką `hiccup`, możesz użyć pobranych danych do generowania kodu HTML lub zapisu struktury danych w formacie JSON przy użyciu biblioteki `cheshire`.

# Zobacz również

- [Dokumentacja clj-http](https://github.com/dakrone/clj-http)
- [Dokumentacja http-kit](https://github.com/http-kit/http-kit)
- [Dokumentacja hiccup](https://github.com/weavejester/hiccup)
- [Dokumentacja cheshire](https://github.com/dakrone/cheshire)