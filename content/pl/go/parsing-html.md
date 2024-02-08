---
title:                "Analiza składniowa HTML"
aliases:
- pl/go/parsing-html.md
date:                  2024-02-03T18:05:34.051813-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analiza składniowa HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Parsowanie HTML w Go polega na analizowaniu zawartości plików HTML w celu ekstrakcji danych, manipulacji strukturą lub konwersji HTML na inne formaty. Programiści robią to ze względów takich jak scraping stron internetowych, tworzenie szablonów i wydobywanie danych, wykorzystując silne możliwości współbieżności Go do efektywnej obróbki dużej ilości stron internetowych.

## Jak to zrobić:

Aby przeprowadzić parsowanie HTML w Go, zazwyczaj używa się pakietu `goquery` lub standardowej biblioteki `net/html`. Oto podstawowy przykład użycia `net/html` do wyodrębnienia wszystkich linków z witryny internetowej:

```go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
)

func main() {
    // Pobranie dokumentu HTML
    res, err := http.Get("http://example.com")
    if err != nil {
        panic(err)
    }
    defer res.Body.Close()

    // Analiza dokumentu HTML
    doc, err := html.Parse(res.Body)
    if err != nil {
        panic(err)
    }

    // Funkcja do rekurencyjnego przechodzenia przez DOM
    var f func(*html.Node)
    f = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, a := range n.Attr {
                if a.Key == "href" {
                    fmt.Println(a.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            f(c)
        }
    }

    // Przechodzenie przez DOM
    f(doc)
}
```

Przykładowy wynik (zakładając, że `http://example.com` zawiera dwie linki):

```
http://www.iana.org/domains/example
http://www.iana.org/domains/reserved
```

Ten kod zażąda strony HTML, przeanalizuje ją i rekurencyjnie przejdzie przez DOM, by znaleźć i wyświetlić atrybuty `href` wszystkich tagów `<a>`.

## Głębsze spojrzenie

Pakiet `net/html` dostarcza podstaw do parsowania HTML w Go, bezpośrednio implementując algorytmy tokenizacji i konstrukcji drzewa określone przez standard HTML5. To niskopoziomowe podejście jest potężne, ale może być rozwlekłe dla złożonych zadań.

W przeciwieństwie do tego, pakiet innej firmy `goquery`, inspirowany przez jQuery, oferuje interfejs wyższego poziomu, który upraszcza manipulację i przechodzenie przez DOM. Pozwala programistom pisać zwięzły i ekspresyjny kod do zadań takich jak wybór elementów, ekstrakcja atrybutów i manipulacja treścią.

Jednak wygoda `goquery` wiąże się z kosztem dodatkowej zależności i potencjalnie wolniejszą wydajnością z powodu swojej warstwy abstrakcyjnej. Wybór między `net/html` a `goquery` (lub innymi bibliotekami parsującymi) zależy od konkretnych wymagań projektu, takich jak potrzeba optymalizacji wydajności czy łatwości użycia.

Historycznie, parsowanie HTML w Go ewoluowało od podstawowych operacji na łańcuchach znaków do zaawansowanej manipulacji drzewem DOM, odzwierciedlając rozwijający się ekosystem języka i zapotrzebowanie społeczności na solidne narzędzia do scrapingu stron internetowych i wydobywania danych. Pomimo możliwości natywnych, powszechność bibliotek firm trzecich jak `goquery` podkreśla preferencje społeczności Go dla modułowego, wielokrotnie używalnego kodu. Jednak dla aplikacji krytycznych pod względem wydajności, programiści mogą nadal preferować pakiet `net/html` czy nawet uciekać się do regex dla prostych zadań parsowania, mając na uwadze ryzyko i ograniczenia parsowania HTML za pomocą regex.
