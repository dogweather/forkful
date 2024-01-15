---
title:                "Pobieranie strony internetowej"
html_title:           "Elm: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Dlaczego Pobieranie Strony Internetowej w Elm Jest Tym, co Potrzebujesz

Pobieranie strony internetowej jest niezbędne dla wielu aplikacji internetowych, takich jak aplikacje do przeglądania internetu czy platformy e-commerce. W Elm, możemy to zrobić w prosty i wydajny sposób, co pozwala nam na przenoszenie kodu na wiele platform, bez konieczności stosowania dodatkowych bibliotek. 

## Jak to Zrobić

Aby pobrać stronę internetową w Elm, musimy najpierw użyć wbudowanej funkcji `Http.get`, która pozwala na wysłanie żądania HTTP do określonego adresu URL. Następnie, musimy określić rodzaj oczekiwanej odpowiedzi, na przykład dane tekstowe czy dane w formacie JSON.

```
Elm.Http.get "https://www.example.com/" stringDecoder
```

W powyższym przykładzie, funkcja `Http.get` pobiera stronę internetową pod adresem `https://www.example.com/` i używa funkcji `stringDecoder` do przetworzenia odpowiedzi na tekst.

## Deep Dive

W Elm, pobieranie strony internetowej odbywa się asynchronicznie, co oznacza, że nie blokuje działania programu. Aby obsłużyć otrzymaną odpowiedź, musimy użyć funkcji `Http.send` wraz z odpowiednimi handlerami.

```
Http.send responseHandler (Elm.Http.get "https://www.example.com/" stringDecoder)
```

W powyższym przykładzie, funkcja `Http.send` oczekuje dwóch argumentów: handlera odpowiedzi i żądania `Http.get`. Handler odpowiedzi jest funkcją, która zostanie wywołana, gdy otrzymamy odpowiedź od serwera.

## Zobacz także

- [Dokumentacja Elm - Pobieranie zasobów](https://package.elm-lang.org/packages/elm/http/latest/Http)
- [Poradnik wideo - Łączenie z internetem w Elm](https://www.youtube.com/watch?v=icE78nl3RD0)
- [Kurs wideo - Elm - Pobieranie zasobów](https://egghead.io/lessons/elm-getting-resourceful-in-elm-with-http-get-and-decode)