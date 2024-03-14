---
date: 2024-01-26 04:35:36.755185-07:00
description: "XML, abr\xE9viation de eXtensible Markup Language, est comme le cousin\
  \ verbeux de JSON. Vous aurez \xE0 vous d\xE9battre avec XML lors de la manipulation\
  \ de\u2026"
lastmod: '2024-03-13T22:44:57.529708-06:00'
model: gpt-4-0125-preview
summary: "XML, abr\xE9viation de eXtensible Markup Language, est comme le cousin verbeux\
  \ de JSON. Vous aurez \xE0 vous d\xE9battre avec XML lors de la manipulation de\u2026"
title: Travailler avec XML
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
XML, abréviation de eXtensible Markup Language, est comme le cousin verbeux de JSON. Vous aurez à vous débattre avec XML lors de la manipulation de systèmes hérités, de logiciels d'entreprise ou d'APIs qui ont fait l'impasse sur le wagon JSON. C'est essentiel pour l'échange de données où XML tient bon la barre.

## Comment faire :
En Rust, vous pouvez gérer XML avec des crates telles que `xml-rs`. Installez en ajoutant `xml-rs = "0.8"` à votre `Cargo.toml`. Voici comment parser un XML simple :

```rust
extern crate xml;

use xml::reader::{EventReader, XmlEvent};

fn main() {
    let xml_data = r#"<book category="fiction">
    <title>Rust in Action</title>
    <author>Tim McNamara</author>
    <year>2021</year>
</book>"#;

    let parser = EventReader::from_str(xml_data);
    for e in parser {
        match e {
            Ok(XmlEvent::StartElement { name, .. }) => {
                println!("Début : {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Texte : {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Fin : {}", name);
            }
            Err(e) => {
                println!("Erreur : {}", e);
            }
            _ => {}
        }
    }
}
```

Sortie :
```
Début : book
Début : title
Texte : Rust in Action
Fin : title
Début : author
Texte : Tim McNamara
Fin : author
Début : year
Texte : 2021
Fin : year
Fin : book
```
Ce code lit en flux le XML, en gérant les éléments de début et de fin plus les données textuelles, et en enregistrant chaque étape.

## Plongée profonde :
XML est un ancien en années technologiques, conçu pour le web à la fin des années 90. Sa conception favorise la lisibilité (tant pour les machines que pour les humains) et des données auto-descriptives étendues.

Des alternatives ? Bien sûr, JSON est le choix moderne pour les API web, plus léger et moins bruyant. Pendant ce temps, YAML a gagné des adeptes pour les configurations, avec sa présentation épurée. Mais XML ne disparaîtra pas de sitôt - d'immenses infrastructures sont construites sur son dos.

Sous le capot, l'analyse XML de Rust s'appuie sur des motifs d'itérateur, gardant l'utilisation de la mémoire faible et les performances affûtées. Vous trouverez des crates comme `serde-xml-rs` pour une expérience plus proche de serde - une aubaine pour ceux habitués à la manipulation de JSON.

## Voir aussi :
Pour plus d'informations sur Rust et XML : 
- `serde-xml-rs` pour la compatibilité serde de Rust : [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Documentation officielle de Rust (car ça ne fait jamais de mal de se remettre à jour) : [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
