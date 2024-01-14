---
title:                "Fish Shell: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą lub tworzysz swoją pierwszą stronę internetową, na pewno musiałeś już zetknąć się z koniecznością wysyłania żądań HTTP. W tym artykule dowiesz się, jak to zrobić przy użyciu języka programowania Fish Shell.

## Jak to zrobić

```Fish Shell
set url "https://example.com/api"
set headers -X POST -H "Content-Type: application/json"
set body {"name": "John", "age": 25}
curl $headers $body $url
```

To kod przykładowej prostej aplikacji, która wysyła żądanie POST na wybrany adres URL, zawierające nagłówki oraz ciało żądania w formacie JSON. Po wykonaniu tego kodu, powinieneś otrzymać odpowiedź ze strony, co potwierdzi wysłanie żądania.

## Deep Dive

Wysyłanie żądań HTTP może być jednak nieco bardziej skomplikowane i wymagać użycia pewnych zaawansowanych opcji. Fish Shell oferuje dużo możliwości dostosowania żądań, na przykład ustawianie nagłówków, wysyłanie żądań z wybraną metodą (GET, POST, PUT, DELETE) czy nawet dostosowywanie żądań do konkretnego serwera. Poprzez wykorzystanie flagi `-I` możesz sprawdzić jedynie nagłówki odpowiedzi, a dodanie flagi `-v` wyświetli dane przesyłane podczas żądania.

## Zobacz też

Jeśli chcesz dowiedzieć się więcej o wysyłaniu żądań HTTP za pomocą języka Fish Shell, warto zapoznać się z poniższymi artykułami:

- [Dokumentacja oficjalna Fish Shell](https://fishshell.com/docs/current/commands.html#-X)
- [Artykuł na temat wysyłania żądań HTTP przez Fish Shell](https://overthewire.org/wargames/narnia/narnia5.html)

Dzięki tym źródłom informacji będziesz mógł lepiej poznać tę funkcjonalność języka Fish Shell i wykorzystać ją w swojej pracy. Powodzenia!