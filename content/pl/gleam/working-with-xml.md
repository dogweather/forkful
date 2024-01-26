---
title:                "Praca z XML"
date:                  2024-01-26T04:31:21.506433-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z XML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/working-with-xml.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Praca z XML obejmuje parsowanie, manipulowanie i generowanie dokumentów XML, które są używane do wymiany danych ze względu na ich strukturalność i powszechność formatu. Programiści obsługują XML, by łączyć się z niezliczonymi systemami, gdzie XML jest lingua franca danych.

## Jak to zrobić:
Gleam natywnie nie wspiera XML-a, więc użyjemy zewnętrznej biblioteki, takiej jak `gleam_xml`. Najpierw dodaj ją do swojego `gleam.toml`:

```toml
[dependencies]
gleam_xml = "~> 1.0"
```

Teraz parsuj i twórz XML:

```rust
import gleam/xml

// Parsuj XML
let doc = xml.parse("<note><to>Tove</to><from>Jani</from></note>")?

// Twórz XML
let node = xml.Element(
  "note",
  [],
  [
    xml.Element("to", [], [xml.Text("Tove")]),
    xml.Element("from", [], [xml.Text("Jani")])
  ]
)
let xml_string = xml.render(node)
```

Przykładowy wynik dla `xml.render(node)` to:

```xml
<note><to>Tove</to><from>Jani</from></note>
```

## Szczegółowe omówienie
XML oznacza eXtensible Markup Language, specyfikacja od W3C, siostrzana dla HTML. Istnieje od końca lat 90. Dla Gleam, obsługa XML-a wydaje się trochę jak krok wstecz w czasie. JSON i bufor protokołów są modniejsze, ale rozległe użycie XML-a w systemach dziedzicznych i niektórych branżach oznacza, że nadal jest aktualny.

Alternatywy takie jak `xmerl` istnieją w ekosystemie Erlanga; jednakże, biblioteka `gleam_xml` oferuje podejście bardziej idiomatyczne dla użytkowników Gleam. Jest zbudowana na istniejących bibliotekach Erlanga, ale eksponuje API przyjazne dla Gleam. Podejście Gleam do XML-a ma na celu prostotę i bezpieczeństwo, redukując boilerplate i podkreślając bezpieczeństwo typów.

Pod względem implementacji, biblioteki XML, w tym `gleam_xml`, zwykle dostarczają struktury podobne do DOM. Obejmuje to węzły, atrybuty i zagnieżdżone elementy, wykorzystując dopasowanie wzorców Erlanga i modele współbieżności do obsługi potencjalnie dużych i skomplikowanych dokumentów.

## Zobacz również
- Biblioteka `gleam_xml` na Hex: [https://hex.pm/packages/gleam_xml](https://hex.pm/packages/gleam_xml)
- Oficjalna specyfikacja XML przez W3C: [https://www.w3.org/XML/](https://www.w3.org/XML/)
- Kompleksowy poradnik XML: [https://www.w3schools.com/xml/](https://www.w3schools.com/xml/)
- Dokumentacja `xmerl` Erlanga dla przetwarzania XML: [http://erlang.org/doc/apps/xmerl/](http://erlang.org/doc/apps/xmerl/)