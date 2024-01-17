---
title:                "Analiza html."
html_title:           "Rust: Analiza html."
simple_title:         "Analiza html."
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/rust/parsing-html.md"
---

{{< edit_this_page >}}

# Czym jest parsowanie HTML i dlaczego programiści to robią?

Parsowanie HTML to proces analizowania kodu HTML, czyli języka wykorzystywanego do tworzenia stron internetowych. Programiści często wykonują ten proces, aby wyodrębnić informacje ze stron internetowych, takie jak nagłówki, treści, linki czy obrazy.

# Jak to zrobić?

Parsowanie HTML w Rust można wykonać za pomocą biblioteki o nazwie "html5ever". Przyjrzyjmy się przykładowemu kodowi:

```Rust
use html5ever::parse_document;
use html5ever::rcdom::{Document, Doctype, Element, RcDom};
use html5ever::tendril::TendrilSink;

fn parse_html(html: &str) {
    let mut dom = parse_document(RcDom::default(), Default::default())
        .from_utf8()
        .read_from(&mut html.as_bytes())
        .unwrap();

    let doctype = dom.document.doctype.as_ref().map(|d: &Doctype| d.name.as_ref().to_string());

    let mut children = dom.document.children.borrow_mut();

    match children.next().unwrap().node {
        Element(ref name, _, _) => assert_eq!(name.local, local_name!("html")),
        _ => unreachable!(),
    }

    match children.next().unwrap().node {
        Element(ref name, _, _) => assert_eq!(name.local, local_name!("head")),
        _ => unreachable!(),
    }

    match children.next().unwrap().node {
        Element(ref name, _, _) => assert_eq!(name.local, local_name!("body")),
        _ => unreachable!(),
    }

    assert!(children.next().is_none());
}
```

W tym przykładzie stosujemy bibliotekę "html5ever" do parsowania dokumentu HTML i wyodrębnienia jego elementów. Jest to tylko jedna z wielu dostępnych bibliotek w Rust, które umożliwiają parsowanie HTML.

# Głębsza analiza

Parsowanie HTML jest istotne dla tworzenia skutecznych i wydajnych aplikacji internetowych. Dzięki temu możemy łatwo pobierać informacje ze stron internetowych i wykorzystywać je w naszych programach.

Alternatywą dla parsowania HTML jest używanie gotowych API dostarczanych przez niektóre serwisy internetowe. Jednak w przypadku budowania aplikacji, które wymagają pobierania różnych danych z różnych stron internetowych, wykorzystanie własnego parsera może być bardziej efektywne.

W implementacji parsowania HTML ważne jest uzyskanie precyzyjnego i zgodnego ze standardem wyniku. Dlatego też wiele bibliotek w Rust jest regularnie aktualizowanych, aby zapewniać wysoką jakość parsowania.

# Zobacz także

- [Dokumentacja biblioteki "html5ever"](https://docs.rs/html5ever)
- [Porównanie wydajności parserów HTML w Rust](https://github.com/petdance/html-parsers-benchmark)
- [Artykuł o parsowaniu HTML w Rust na blogu "The Rust Programming Language"](https://doc.rust-lang.org/book/first-edition/parsing.html)