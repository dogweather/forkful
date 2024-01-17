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

### Co & Dlaczego?

Pobieranie stron internetowych jest procesem pozwalającym na pobieranie informacji lub danych z określonej strony internetowej. Programiści często wykonują tę czynność, aby uzyskać dostęp do potrzebnych informacji lub zasobów, aby móc wykorzystać je w swoich projektach.

### Jak to zrobić:

```Elm 
import Http

Http.getString "https://wyimaginowanastrona.com" 
    |> Task.perform (\result ->
        case result of
            Ok response ->
                Debug.log "Pobrane dane:" response.body

            Err error ->
                Debug.log "Wystąpił błąd:" error
        )
```

### Głębszy zanurzenie:

Pobieranie stron internetowych jest powszechnie stosowane w celu uzyskania danych do analizy lub wykorzystania w aplikacjach internetowych. Alternatywnym sposobem na pobieranie stron internetowych jest wykorzystanie narzędzi takich jak Selenium lub Puppeteer, które symulują działanie przeglądarki i umożliwiają wypełnienie formularzy lub kliknięcie na interaktywne elementy. W Elm, proces pobierania stron internetowych jest wykonywany przy pomocy komendy Http.getString. Ta funkcja zwraca Task, który może być wykonany przy użyciu funkcji Task.perform.

### Zobacz także:

- Dokumentacja Elm, https://guide.elm-lang.org/
- Elm HTTP Package, http://package.elm-lang.org/packages/elm-lang/http/latest/