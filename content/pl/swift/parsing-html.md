---
title:                "Analiza html"
html_title:           "Swift: Analiza html"
simple_title:         "Analiza html"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz tworzyć aplikacje, które pobierają dane z internetu, to słowo "parsing HTML" możliwe, że już usłyszałeś. Jest to proces, w którym kod jest wykorzystywany do analizowania strony internetowej i pobierania odpowiednich informacji. Czytaj dalej, aby dowiedzieć się dlaczego jest to przydatne i jak to zrobić w języku Swift.

## Jak to zrobić

Aby rozpocząć parsowanie HTML w języku Swift, będziesz potrzebować dwóch rzeczy: biblioteki HTML i wiedzy na temat podstawowej składni Swift.

```Swift
import SwiftSoup // importowanie biblioteki HTML
let html = "<p>Hello, world!</p>" // przykładowy kod HTML
do {
  let doc: Document = try SwiftSoup.parse(html) // parsowanie
  let text: String? = try doc.select("p").text() // pobieranie tekstu ze znacznika <p>
  print(text) // wypisanie tekstu
} catch Exception.Error(let type, let message) { // obsługa wyjątków
  print(message)
} catch {
  print("error")
}
```
Output:
```
Hello, world!
```

Możesz również użyć bardziej złożonej struktury HTML, na przykład zagnieżdżonych znaczników:

```Swift
import SwiftSoup
let html = "<div><h1>Swift is cool!</h1><p>Swift is a powerful and easy to learn language.</p></div>"
do {
  let doc: Document = try SwiftSoup.parse(html)
  let text: String? = try doc.select("h1").text()
  let paragraph: String? = try doc.select("p").text()
  print(text + " " + paragraph)
} catch Exception.Error(let type, let message) {
  print(message)
} catch {
  print("error")
}
```
Output:
```
Swift is cool! Swift is a powerful and easy to learn language.
```

## Deep Dive

Aby zrozumieć lepiej, jak działa parsowanie HTML w języku Swift, musisz poznać podstawy składni tego języka. Jest on oparty na algorytmie tzw. "drzewa DOM", który jest wykorzystywany do przetwarzania dokumentów HTML.

Kluczową częścią tego procesu jest pomocnicza biblioteka SwiftSoup, która dostarcza wiele przydatnych narzędzi do parsowania HTML. Możesz na przykład użyć metody .select() do wybierania konkretnego elementu ze strony internetowej, wykorzystując selektory CSS.

Jeśli chcesz pogłębić swoją wiedzę na temat parsowania HTML w języku Swift, istnieje wiele dostępnych tutoriali i dokumentacji, dzięki którym możesz nauczyć się więcej na ten temat.

## Zobacz też
- [Dokumentacja SwiftSoup](https://swiftsoup.codershigh.com/docs/index.html)
- [Tutorial "How to Parse HTML on iOS with Swift and SwiftSoup"](https://codershigh.dscloud.biz/js/278)