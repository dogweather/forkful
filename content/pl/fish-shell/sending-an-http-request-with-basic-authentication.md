---
title:                "Wysyłanie żądania http z podstawową autoryzacją"
html_title:           "Fish Shell: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/fish-shell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP z podstawową autoryzacją to proces, w którym programista używa swojego identyfikatora i hasła do autoryzacji do zasobów sieciowych. Programiści często wykonują to w celu uzyskania dostępu do chronionych stron internetowych lub API.

## Jak to zrobić:

```Fish Shell
curl --user "twoje_identyfikator:twoje_hasło" URL_strony
```

Output będzie zawierał odpowiedź ze strony lub API, które próbujesz uzyskać dostęp.

## Głębsze wniknięcie:

Wysyłanie żądania HTTP z podstawową autoryzacją jest powszechną praktyką programistów do uzyskania dostępu do zasobów sieciowych. W przeszłości technika ta była wykorzystywana głównie do autoryzacji dostępu do stron internetowych, ale obecnie jest wykorzystywana również do autoryzacji dostępu do API. Istnieją również inne sposoby na wysyłanie żądania HTTP z autoryzacją, takie jak oAuth, ale podstawowa autoryzacja nadal jest popularna ze względu na swoją prostotę.

## Zobacz także:

Poniżej znajdują się linki do źródeł związanych z wysyłaniem żądania HTTP z podstawową autoryzacją:

- [Dokumentacja Fish Shell](https://fishshell.com/docs/current/commands.html#curl)
- [Podstawowa autoryzacja HTTP](https://developer.mozilla.org/pl/docs/Web/HTTP/Authentication) 
- [Alternatywne metody autoryzacji HTTP](https://www.redhat.com/en/topics/api/what-is-basic-authentication)