---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'analyse syntaxique HTML concerne la conversion des fichiers HTML en une structure de données compréhensible par votre programme. Les programmeurs l'effectuent pour manipuler, extraire des données, et interagir avec le contenu web.

## Comment faire :

Voici un exemple simple de comment utiliser le crate html5ever de Rust pour analyser un document HTML.

```Rust
use html5ever::driver::ParseOpts;
use html5ever::tendril::TendrilSink;

fn main() {
    let html = "<html><body>Rust est super!</body></html>";
    let dom = kuchiki::parse_html().one(html);
    println!("{:#?}", dom);
}
```

L'exécution de ce code affichera une structure DOM que vous pouvez explorer.

```Rust
Document {
    node: Node {
        parent: None,
        previous_sibling: None,
        next_sibling: None,
        first_child: Some(Doctype),
        last_child: Some(Element),
        data: Document,
    },
    encoding_name: UTF-8,
    url: None,
    quirks_mode: NoQuirks,
}
```

## Plongée profonde :

L'analyse syntaxique HTML existe depuis les débuts du web, mais elle est devenue plus complexe avec l'évolution des standards HTML. En Rust, les alternatives à html5ever incluent html-parser et select.rs. Cependant, html5ever est largement reconnu pour son exhaustivité et sa conformité aux spécifications HTML5. Il convertit le document HTML en arbre d'objets document (DOM), qui peut être exploré et modifié par le programmeur.

## Voir aussi :

- Documentation html5ever : https://html5ever.org/
- Crate html-parser : https://crates.io/crates/html-parser
- Crate select.rs : https://crates.io/crates/select