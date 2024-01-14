---
title:                "Rust: Das Parsen von html"
simple_title:         "Das Parsen von html"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

Das Parsen von HTML ist eine wesentliche Fähigkeit für jeden Webentwickler. Ohne diese Fähigkeit können wir keine effektiven Webanwendungen erstellen, da HTML die strukturierte Grundlage jedes skriptgesteuerten Inhalts ist. Mit Rust können wir jedoch auf einfache und effiziente Weise HTML parsen, was uns dabei hilft, robuste und schnelle Webanwendungen zu erstellen.

## Wie

Um HTML mit Rust zu parsen, müssen wir zuerst das `html5ever`-Paket installieren. Dann können wir mit dem folgenden Codebeispiel eine HTML-Datei parsen und die ausgegebenen Nodes und Texte anzeigen:

```Rust
extern crate html5ever;
use html5ever::parse_document;
use html5ever::rcdom::{RcDom, NodeData};
use html5ever::tendril::TendrilSink;

fn parse_html(html: &str) -> RcDom {
   // Parse the HTML string into a DOM tree
   let dom = parse_document(RcDom::default(), Default::default())
        .from_utf8()
        .read_from(&mut html.as_bytes())
        .unwrap();

   dom
}

fn main() {
   // Load a sample HTML file
   let html = include_str!("sample_html.html");

   // Parse the HTML using our function
   let dom = parse_html(html);

   // Iterate through the DOM tree and print nodes and texts
   for node in dom.document.children.borrow().iter() {
        match node.data {
            NodeData::Element { ref name, .. } => println!("Node: {}", name.local),
            NodeData::Text { ref contents } => println!("Text: {}", contents.borrow()),
            _ => (),
        }
    }
}
```

Die Ausgabe dieses Codes wird wie folgt aussehen:

```html
Text: This is a sample HTML file for parsing.
Node: head
Node: title
Text: Sample HTML
Node: meta
Node: body
Node: h1
Text: Hello World!
```

Mit dieser Methode können wir auf einfache Weise durch den DOM-Baum navigieren und die verschiedenen Elemente und Inhalte auslesen.

## Deep Dive

Das Parsen von HTML mit Rust ist dank des `html5ever`-Pakets sehr schnell, da es auf einer C++-Implementierung von HTML5 basiert. Dies ermöglicht es uns, komplexe HTML-Dokumente effizient zu parsen. Zudem bietet Rust eine hohe Sicherheit und Verlässlichkeit, wodurch vermeidbare Abstürze oder Fehler im Code vermieden werden können.

Ein interessantes Feature von `html5ever` ist die Fähigkeit, benutzerdefinierte Elemente und Attribute zu erstellen und zu parsen. Dies ermöglicht es uns, unsere eigenen HTML-Tags und -Eigenschaften zu definieren, die in andere Dateien geparst werden können.

## Siehe auch

- [Offizielle Dokumentation zu html5ever](https://docs.rs/html5ever/0.24.0/html5ever/index.html)
- [Beispiele für das Parsen von HTML mit Rust](https://github.com/rust-lang-nursery/html5ever/tree/master/examples)
- [Weitere Informationen über Rust](https://www.rust-lang.org/de/)