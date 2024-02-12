---
title:                "Trabalhando com XML"
date:                  2024-01-26T04:35:59.971439-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com XML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-xml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
XML, abreviação para eXtensible Markup Language, é como o primo verborrágico do JSON. Você terá que lidar com XML ao trabalhar com sistemas legados, softwares empresariais, ou APIs que não aderiram à onda do JSON. É essencial para a troca de dados onde o XML mantém sua fortaleza.

## Como fazer:
Em Rust, você pode manipular XML com crates como `xml-rs`. Instale adicionando `xml-rs = "0.8"` ao seu `Cargo.toml`. Aqui está como analisar um XML simples:

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
                println!("Início: {}", name);
            }
            Ok(XmlEvent::Characters(data)) => {
                println!("Texto: {}", data);
            }
            Ok(XmlEvent::EndElement { name }) => {
                println!("Fim: {}", name);
            }
            Err(e) => {
                println!("Erro: {}", e);
            }
            _ => {}
        }
    }
}
```

Saída:
```
Início: book
Início: title
Texto: Rust in Action
Fim: title
Início: author
Texto: Tim McNamara
Fim: author
Início: year
Texto: 2021
Fim: year
Fim: book
```
Este código lê fluxos de XML, lidando com elementos de início e fim, além de dados de texto, registrando cada etapa.

## Aprofundando:
O XML é um veterano nos anos tecnológicos, criado para a web no final dos anos 90. Seu design promove a legibilidade (tanto para máquinas quanto para humanos) e dados auto-descritivos extensos.

Alternativas? Claro, o JSON é a escolha moderna para APIs web, mais leve e menos ruidoso. Enquanto isso, o YAML conquistou fãs para configurações, com seu layout limpo. Mas o XML não vai desaparecer tão cedo—vastas infraestruturas são construídas em sua base.

Por baixo do capô, a análise de XML em Rust se apoia em padrões de iteradores, mantendo o uso de memória baixo e o desempenho afiado. Você encontrará crates como `serde-xml-rs` para uma experiência mais próxima do serde—uma bênção para aqueles acostumados com o manuseio do JSON.

## Veja Também:
Para mais sobre Rust e XML: 
- `serde-xml-rs` para compatibilidade do Rust com serde: [https://github.com/RReverser/serde-xml-rs](https://github.com/RReverser/serde-xml-rs)
- Documentação oficial do Rust (porque nunca é demais reciclar): [https://doc.rust-lang.org/stable/book/](https://doc.rust-lang.org/stable/book/)
