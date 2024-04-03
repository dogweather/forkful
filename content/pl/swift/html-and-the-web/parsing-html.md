---
date: 2024-01-20 15:33:59.514523-07:00
description: "Parsing HTML oznacza proces analizowania struktury kodu HTML, aby wydoby\u0107\
  \ z niego konkretne dane. Programi\u015Bci robi\u0105 to, by pozyska\u0107 informacje\
  \ ze stron\u2026"
lastmod: '2024-03-13T22:44:35.752767-06:00'
model: unknown
summary: "Parsing HTML oznacza proces analizowania struktury kodu HTML, aby wydoby\u0107\
  \ z niego konkretne dane."
title: Przetwarzanie HTML
weight: 43
---

## What & Why? (Co & Dlaczego?)
Parsing HTML oznacza proces analizowania struktury kodu HTML, aby wydobyć z niego konkretne dane. Programiści robią to, by pozyskać informacje ze stron internetowych, które nie oferują API, lub dla szybkiej transformacji treści HTML na inne formaty.

## How to: (Jak to zrobić:)
Swift nie ma wbudowanej obsługi parsowania HTML, więc wykorzystamy bibliotekę zewnętrzną, jak `SwiftSoup`. Aby jej użyć, musisz dodać zależność do swojego pliku `Package.swift`:

```swift
.package(url: "https://github.com/scinfu/SwiftSoup.git", from: "2.3.2")
```

Teraz, możesz spróbować prostego parsowania tytułu strony:

```swift
import SwiftSoup

let html = "<html><head><title>Witaj, Swift!</title></head></html>"
do {
    let doc = try SwiftSoup.parse(html)
    if let title = try doc.title() {
        print(title)
    }
} catch {
    print("Nie udało się sparsować HTML-a: \(error)")
}

// Wydruk w konsoli: "Witaj, Swift!"
```

## Deep Dive (Zagłębiamy się)
Parsowanie HTML w Swift to jeszcze młoda dziedzina. Inne języki, jak Python z biblioteką BeautifulSoup, mają bardziej rozwinięte narzędzia. Historia parsowania HTML zaczyna się jednak dużo wcześniej i rozwija wraz z webem. Alternatywami dla SwiftSoup mogą być inne biblioteki takie jak Fuzi czy Kanna, które także wykorzystują XPath i CSS selectors. Ważne jest, żeby pamiętać o potencjalnych problemach przy parsowaniu skomplikowanej i zmieniającej się zawartości HTML, jak dynamicznie generowane strony.

Implementacja parsowania jest różna - może być na podstawie DOM, SAX, albo innego modelu. SwiftSoup stara się dostarczyć API podobne do silnika przeglądarki, z tą różnicą, że działamy w kontekście aplikacji Swift.

## See Also (Zobacz także)
- SwiftSoup GitHub: https://github.com/scinfu/SwiftSoup
- Dokumentacja SwiftSoup: https://scinfu.github.io/SwiftSoup/
- BeautifulSoup w Pythonie: https://www.crummy.com/software/BeautifulSoup/
- "Web Scraping with Swift" Tutorial: https://www.raywenderlich.com/567-urlsession-tutorial-getting-started
