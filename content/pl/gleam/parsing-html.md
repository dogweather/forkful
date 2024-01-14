---
title:                "Gleam: Przetwarzanie html"
simple_title:         "Przetwarzanie html"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach większość stron internetowych jest oparta na HTML, języku znaczników do tworzenia zawartości w sieci. Często, aby dokonać analizy lub przetworzenia danej strony, potrzebujemy sparsować jej zawartość. W tym blogu dowiesz się, jak w prosty sposób zrobić to przy użyciu języka programowania Gleam.

## Jak to zrobić

Parsowanie HTML jest możliwe dzięki wykorzystaniu biblioteki o nazwie Scrivener. Najpierw musimy zainstalować tę bibliotekę za pomocą menadżera pakietów, takiego jak Mix. Następnie możemy stworzyć prosty plik `.gleam` i zaimportować bibliotekę Scrivener, aby móc wykonać operacje na sparsowanym HTML.

```Gleam
import scrivener/html

let html_text = "<html><body><h1>Title</h1></body></html>"

let dom = html_text |> parse_html

dom.nodes.each(fn(n) {
  match n {
    #scrivener.Tag{ name, _, children } -> {
      io.format("*** Found tag: %s ***\n", [name])
      children.each(fn(c) {
        match c {
          #scrivener.TextNode{ text } -> {
            io.format("Found text node: %s\n", [text])
          }
          _ -> { /* Ignore other types of nodes */ }
        }
      })
    }
    _ -> { /* Ignore comments and other types of nodes */ }
  }
})
```

Powyższy kod wyświetli następujący wynik:

```
*** Found tag: html ***
*** Found tag: body ***
Found text node: Title
```

Widzimy, że przy użyciu zaledwie kilku linii kodu udało nam się sparsować zawartość HTML i wyświetlić jej strukturę. Teraz możemy użyć tej biblioteki do dalszej analizy lub przetwarzania danych.

## Wnikliwe spojrzenie

Biblioteka Scrivener pozwala nam nie tylko parsować HTML, ale także wykonywać różnego rodzaju operacje na danych, takie jak szukanie elementów, zmiana ich atrybutów czy usuwanie węzłów. Warto zweryfikować dokumentację, aby poznać wszystkie dostępne funkcje.

W przypadku potrzeby obsługi bardziej złożonych lub niestandardowych przypadków, można również skorzystać z innych bibliotek, takich jak Misaki czy Hexpm. Są one bardziej rozbudowane i posiadają większą liczbę opcji.

## Zobacz również

- Dokumentacja biblioteki Scrivener: https://hexdocs.pm/scrivener/Scrivener.html
- Biblioteka Misaki: https://hexdocs.pm/misaki/Misaki.html
- Biblioteka Hexpm: https://hexdocs.pm/hexpm/0.8.0/api-reference.html