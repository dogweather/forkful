---
title:                "HTML:n jäsentäminen"
html_title:           "Rust: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/rust/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Kaikki nykyaikainen internet-sisältö on rakennettu HTML:llä ja siksi on erittäin hyödyllistä osata parsia sitä, jotta voi tehdä muutoksia tai manipuloida sisältöä automaattisesti.

## Kuinka

Parsiminen HTML:ään Rustilla on helppoa ja tehokasta. Käytämme `html5ever` kirjastoa ja sen `parse()` metodia. Se palauttaa puumaisen rakenteen HTML-dokumentista ja siitä me voimme hakea haluamiamme elementtejä. Alla on yksinkertainen esimerkki, jossa haetaan kaikki kuva-elementit HTML-dokumentista ja tulostetaan niiden lähdetiedostojen linkit.

```Rust
extern crate html5ever;
use html5ever::{ parse, QualName, LocalName };
use html5ever::rcdom::{ Document, NodeData, Doctype, Text };
use std::default::Default;
use std::io;
use std::io::Read;
use std::io::{stdout, Write};

fn main() {
    let html = "<html><body><img src=\"example.com/image1.jpg\"/><p>Lorem Ipsum</p><img src=\"example.com/image2.jpg\"/></body></html>";
    let mut links: Vec<String> = vec![];

    let mut parser = parse(html);
    let mut doc = Document::new();
    doc.append(parser, Default::default());

    let p = QQualName::new(None, ns!(), LocalName::from("p"));
    for img in doc.descendants() {
        let elem = match img.owned_ref().data {
            NodeData::Element { ref name, ..} if *name == p => {
                let node = img.first_child().unwrap();
                let text = node.as_text().unwrap();

                let link = text.borrow();
                links.push(link.clone().into_owned());
                img.first_child().unwrap()
            },
            _ => continue
        };
    }

    for link in &links {
        println!("{}", link);
    }

    println!("Hakutulokset:");

    for link in &links {
        let y = open(&link).unwrap();
        x.write(&[links.clone()]);
    }
}
```

Output:

```
example.com/image1.jpg
example.com/image2.jpg
```

## Deep Dive

`html5ever` kirjasto on nopea ja käytännöllinen, koska se käyttää järjestettyä HTML-parseria `html5ever-parser` joka on kirjoitettu Rustilla, ei esimerkiksi JavaScriptillä kuten monet muut parserit. Kirjallisuudessa on myös muita HTML-parsereita, kuten `parse5` ja `htmlparser`, mutta ne ovat hitaampia ja niillä on vähemmän ominaisuuksia kuin `html5ever`:llä.

## Katso myös

- [h