---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej to proces, w którym zawartość konkretnej strony web jest zapisywany na dysku twardym. Programiści robią to, żeby mieć lokalny dostęp do stron, analizować struktury HTML itd.

## Jak to zrobić:

```Fish Shell
# użyj “curl” do pobrania strony
curl -o nazwa_pliku.html https://twojastrona.com
```
Tym samym poleceniem pobierasz stronę internetową, zapisując ją pod nazwą `nazwa_pliku.html`.

```Fish Shell
# Przykładowe wyjście
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100  200k  100  200k    0     0  4545k      0 --:--:-- --:--:-- --:--:-- 4545k
```

## Zanurkowanie

Pobieranie stron internetowych ma swoje korzenie w czasach, gdy prędkość internetu była zbyt wolna, aby przeglądać strony online. Teraz służy do wielu celów, takich jak automatyzacja, badania itp.

Alternatywą dla `curl` jest `wget`, które także jest często używane do pobierania stron. Obie te komendy mają różne opcje dostosowania.

Za pobraniem strony internetowej za pomocą `curl` stoi wiele mechanizmów, takich jak nawiązywanie połączenia TCP, wysyłanie żądania HTTP, odbieranie odpowiedzi itd.

## Zobacz też

- [curl vs wget](https://daniel.haxx.se/docs/curl-vs-wget.html): artykuł porównujący curl i wget.
- [Fish Shell dokumentacja](https://fishshell.com/docs/current/index.html): oficjalna dokumentacja Fish Shella.
- [HTTP - jak to działa](https://developer.mozilla.org/pl/docs/Web/HTTP/Overview): Artykuł MDN tłumaczący, jak działa HTTP.