---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pobieranie strony internetowej to proces zapisywania jej zawartości na dysku twardym. Programiści robią to, aby analizować kod HTML strony, testować interakcje z API lub gromadzić dane do późniejszej analizy.

## Jak to zrobić:

Aby pobierać strony internetowe za pomocą Bash, można wykorzystać popularne narzędzia takie jak `curl` lub `wget`. Poniżej znajduje się przykład użycia `curl`.

```Bash
curl "https://www.example.com" -o example.html
```

Po uruchomieniu, ta komenda pobierze stronę internetową `https://www.example.com` i zapisze jej zawartość do pliku `example.html`.

## Zaawansowane Informacje:

Bash jest luźno typowanym językiem powłoki Unix stworzonym przez Briana Foxa w 1989 roku jako ulepszona wersja sh.



Alternatywami dla `curl` i `wget` są narzędzia takie jak `lynx`, `links`, czy `elinks`, które są również dostępne na wielu systemach Unix.

Podczas pobierania stron internetowych, najważniejszym aspektem jest przemiana URL na odpowiednią lokalizację na naszym dysku twardym. Oba narzędzia, `curl` i `wget`, obsługują przekierowania, ciasteczka i inne funkcje HTTP, które mogą być potrzebne przy pobieraniu skomplikowanych stron.

## Zobacz Również:

[Bash Programming](https://tldp.org/HOWTO/Bash-Prog-Intro-HOWTO.html): Wprowadzenie do programowania w Bash.

[Curl Users Guide](https://curl.haxx.se/docs/manpage.html): Przewodnik dla użytkowników narzędzia Curl.

[Wget Manual](https://www.gnu.org/software/wget/manual/wget.html): Dokumentacja narzędzia Wget.