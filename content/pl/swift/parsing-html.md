---
title:                "Analiza składni HTML"
html_title:           "Swift: Analiza składni HTML"
simple_title:         "Analiza składni HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/parsing-html.md"
---

{{< edit_this_page >}}

Co i Dlaczego?

Parsowanie HTML to proces przetwarzania kodu HTML na zrozumiałe dla komputera struktury danych. Programiści często stosują to narzędzie do wydobycia konkretnych informacji z kodu strony internetowej, np. nagłówków czy tabeli danych.

Jak to zrobić:

```Swift
// Załóżmy, że mamy już pobrany kod HTML w zmiennej "html"

let doc = try HTMLDocument(string: html)

// Żeby wydobyć nagłówki ze strony, wystarczy wykonać następujące kroki:

let headerElements = doc.query(xpath: "//h1 | //h2 | //h3")

for element in headerElements {
    print(element.stringValue)
}

```

Wyjście powinno wyglądać mniej więcej tak:

```
Nagłówek pierwszego poziomu
Nagłówek drugiego poziomu
Nagłówek trzeciego poziomu
```

Głębokie zanurzenie:

Parsowanie HTML jest bardzo ważnym procesem w tworzeniu stron internetowych. Pierwsze narzędzia do tego pojawiły się już w latach 90., a od tego czasu stały się niezwykle popularne. Alternatywnie, można też użyć CSS Selectors lub XPath, jednak nie są one tak wszechstronne i wygodne jak metoda z użyciem języka Swift. Implementacja parsera HTML w Swift jest dostępna dzięki bibliotece HTMLKit. Można też użyć wbudowanych funkcji języka, jak na przykład `String.replacingOccurrences`, jednak nie są one tak wydajne jak wykorzystanie narzędzi specjalnie do tego celu.

Zobacz też:

- HTMLKit: https://github.com/vapor-community/html-kit
- Przewodnik po wyrażeniach XPath: https://zvon.org/comp/r/tut-XPath_1.html
- Samouczek Swift: https://developer.apple.com/swift/resources/