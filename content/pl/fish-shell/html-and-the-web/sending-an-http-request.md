---
date: 2024-01-20 17:59:45.964361-07:00
description: "How to: | Jak to zrobi\u0107: W Fish Shell mo\u017Cemy u\u017Cy\u0107\
  \ program\xF3w jak `curl` lub `httpie`. Prosty przyk\u0142ad z `curl`."
lastmod: '2024-04-05T22:37:44.596853-06:00'
model: gpt-4-1106-preview
summary: "| Jak to zrobi\u0107: W Fish Shell mo\u017Cemy u\u017Cy\u0107 program\xF3\
  w jak `curl` lub `httpie`. Prosty przyk\u0142ad z `curl`."
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
weight: 44
---

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
