---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Swift"
category:             "Swift"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/swift/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Przetwarzanie HTML (parsing HTML) to proces ekstrakcji danych z dokumentów HTML. Programiści robią to, aby uzyskać dostęp do istotnych danych umieszczonych na stronach internetowych, które mogą być używane w różnych celach, takich jak web scraping i web automation.

## Jak to zrobić:

Swift nie ma wbudowanej biblioteki do przetwarzania HTML, ale możemy używać zewnętrznej biblioteki, takiej jak Kanna. Oto jak to zrobić:

```Swift
// Importujemy bibilotekę Kanna
import Kanna

func przetwarzanieHTML(nazwaStrony: String) {
  do {
    // Pobieramy zawartość strony
    let zawartosc = try String(contentsOf: URL(string: nazwaStrony)!, encoding: .utf8)
      
    // Tworzymy dokument HTML
    let doc = try HTML(html: zawartosc, encoding: .utf8)
      
    // Wyciągamy interesujące nas dane
    for link in doc.xpath("//a | //link") {
      print(link.text!, link["href"]!)
    }
  } catch let error {
    print("Błąd: \(error)")
  }
}
```

Sample output:

```Swift
"Strona główna", "/index.html"
"Kontakt", "/contact.html"
```

## Deep Dive

Historia przetwarzania HTML sięga początków internetu. Wtedy programiści zaczęli odkrywać potrzebę ekstrakcji danych z dokumentów HTML do różnych celów, takich jak automatyzacja przeglądarek i web scraping.

Inne języki programowania, takie jak Python czy JavaScript, mają również swoje biblioteki do przetwarzania HTML. W przypadku Swifta, użyliśmy biblioteki Kanna, ale istnieją inne, takie jak SwiftSoup czy SwiftHtmlParser.

Szczegóły implementacji parowania HTML zależą od wykorzystanej biblioteki. W przypadku Kanna, używa ona XPath do identyfikacji interesujących nas elementów na stronie. Należy jednak pamiętać, że parowanie HTML jest procesem który może się różnić w zależności od struktury danego dokumentu HTML.

## Zobacz również:

Możesz dowiedzieć się więcej o przetwarzaniu HTML na tych stronach:

- Kanna: https://github.com/tid-kijyun/Kanna
- SwiftSoup: https://github.com/scinfu/SwiftSoup
- XPath: https://www.w3schools.com/xml/xpath_intro.asp
- HTML Parser w Python: https://docs.python.org/3/library/html.parser.html
- HTML Parser w JavaScript: https://developer.mozilla.org/en-US/docs/Web/API/DOMParser