---
title:                "Wysyłanie żądania HTTP"
aliases:
- pl/fish-shell/sending-an-http-request.md
date:                  2024-01-20T17:59:45.964361-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? | Co i Dlaczego?
Wysyłanie żądania HTTP to proces komunikacji z serwerem internetowym, by uzyskać dane lub wykonać operację. Programiści to robią, by współdziałać z API, pobierać treści stron, przesyłać dane formularzy lub testować aplikacje webowe.

## How to: | Jak to zrobić:
W Fish Shell możemy użyć programów jak `curl` lub `httpie`. Prosty przykład z `curl`:

```Fish Shell
curl https://api.example.com/posts
```

Zwraca odpowiedź od API. Jeśli chcesz, żeby było bardziej 'fishy', możesz zrobić funkcję:

```Fish Shell
function fetch_posts
    curl https://api.example.com/posts
end

fetch_posts
```

I przykład z `httpie`:

```Fish Shell
http GET https://api.example.com/posts
```

Sample output:

```sh
HTTP/1.1 200 OK
Content-Type: application/json

[
    {
        "id": 1,
        "title": "Fish Shell dla początkujących",
        "content": "..."
    },
    ...
]
```

## Deep Dive | Do Rzeczy
Zanim pojawił się Fish Shell, programiści używali Bash, Zsh i inne shelle Unixowe do pracy z HTTP. Teraz, mamy więcej wygodnych narzędzi. `curl` jest standardem, lecz `httpie` oferuje przyjazne dla człowieka formatowanie. Fish Shell nie ma wbudowanej obsługi HTTP, ale świetnie komponuje się z zewnętrznymi narzędziami.

Kluczowe jest, że Fish Shell automatyzuje wspólne zadania i skrypty, które mogą zawierać wysyłanie żądań HTTP jako część większego procesu. Możesz tworzyć funkcje w Fish, które zintegrują się z `curl` lub `httpie`, i używać je jak własne polecenia.

## See Also | Zobacz również
- [Dokumentacja `curl`](https://curl.se/docs/)
- [Dokumentacja `httpie`](https://httpie.io/docs)
- [Fish Shell Tutorial](https://fishshell.com/docs/current/tutorial.html)
- [Bardziej zaawansowane skrypty w Fish](https://fishshell.com/docs/current/index.html#scripting)
