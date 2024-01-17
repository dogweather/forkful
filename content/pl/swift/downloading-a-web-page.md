---
title:                "Pobieranie strony internetowej"
html_title:           "Swift: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Ściąganie strony internetowej to proces pobierania zawartości strony z Internetu na komputer lub urządzenie mobilne. Programiści często wykorzystują to narzędzie do pobierania danych lub informacji z wybranych stron internetowych. Może to być wykorzystane do scrapowania danych, automatyzacji zadań lub analizy treści.

## Jak to zrobić:
```Swift
let url = URL(string: "https://www.example.com") // Tworzenie URL do strony internetowej
if let html = try? String(contentsOf: url!) { // Pobieranie zawartości strony jako string
    print(html) // Wyświetlenie pobranej strony w konsoli
}
```

## Głębsze zagadnienia:
Ściąganie stron internetowych jest popularnym narzędziem w programowaniu, szczególnie przy tworzeniu aplikacji do przeglądania internetowego. Takie rozwiązanie jest nie tylko wygodne dla użytkowników, ale również pozwala na wykorzystanie dużej ilości danych z różnych stron internetowych. Alternatywnym sposobem na pobieranie danych z Internetu jest wykorzystanie API (interfejsu programistycznego aplikacji), ale wymaga to wcześniejszej integracji z daną stroną internetową. Implementacja pobierania danych z Internetu w języku Swift jest prosta i wymaga tylko kilku linijek kodu.

## Zobacz także:
- [Wyjaśnienie pojęcia "narzędzie do scrapowania" (ang. web scraping)](https://www.investopedia.com/terms/w/web-scraping.asp)
- [Wykorzystanie API do pobierania danych z Internetu](https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Client-side_web_APIs/Introduction)