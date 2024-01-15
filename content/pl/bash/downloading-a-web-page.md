---
title:                "Pobieranie strony internetowej"
html_title:           "Bash: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie stron internetowych może być przydatne w wielu różnych sytuacjach - od tworzenia kopii zapasowych witryny po analizowanie kodu źródłowego lub pobieranie informacji. W tym artykule dowiesz się, jak używać Bash do pobierania stron internetowych w sposób prosty i efektywny.

## Jak to zrobić

Najprostszym sposobem na pobranie strony internetowej za pomocą Bash jest użycie narzędzia `curl` oraz przekierowanie wyniku do pliku. Oto przykład kodu:

```Bash
curl https://example.com > index.html
```
W powyższym przykładzie używamy komendy `curl` do pobrania strony internetowej `example.com` i przekierowania jej zawartości do pliku `index.html`.

Jeśli chcesz pobrać tylko nagłówki strony, możesz użyć opcji `-I`:

```Bash
curl -I https://example.com
```

Jeśli natomiast chcesz pobrać tylko część strony, na przykład tylko elementy tekstu znajdujące się pomiędzy znacznikami `<body>`, możesz skorzystać z narzędzia `grep`:

```Bash
curl https://example.com | grep '<body>'
```

## Deep Dive

Powyższe przykłady są tylko niewielką częścią możliwości pobierania stron internetowych za pomocą Bash. W rzeczywistości, możesz wykorzystać różne narzędzia i opcje do precyzyjnego pobrania wybranych fragmentów strony.

Na przykład, jeśli chcesz pobierać tylko określone części strony przy użyciu określonych wzorców, możesz skorzystać z narzędzia `awk`. Natomiast, jeśli chcesz pobierać więcej niż jedną stronę naraz, możesz wykorzystać pętlę `for` do wykonania kolejnych zapytań.

## Zobacz także

- [Bash Command Line Cookbook — Downloading Web Pages](https://www.oreilly.com/library/view/bash-command-line/0596009658/ch04s07.html)
- [Bash One-Liners Explained, Part III: All about redirections](https://catonmat.net/bash-one-liners-explained-part-three)
- [Linux Command Line: Downloading Files](https://ryanstutorials.net/linuxtutorial/downloading.php)