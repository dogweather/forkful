---
title:                "Enviando una solicitud http"
html_title:           "Rust: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Enviar una solicitud HTTP es una forma de comunicarse con un servidor web para obtener información o realizar acciones. Los programadores lo hacen para acceder a recursos en línea, como datos y servicios.

## Cómo:
```Rust
use reqwest::blocking::get;

fn main() {
    let response = get("https://example.com").unwrap();
    println!("{}", response.status());

    let body = response.text().unwrap();
    println!("{}", body);
}
```

## Inmersión profunda:
El protocolo HTTP (Protocolo de Transferencia de Hipertexto) fue creado en 1991 y es la base de la comunicación en la web. Existen alternativas como HTTPS (HTTP seguro) que utiliza cifrado para proteger la información. La implementación de una solicitud HTTP en Rust se puede realizar con bibliotecas como reqwest o hyper.

## Ver también:
- [Documentación oficial de reqwest](https://docs.rs/reqwest/)
- [Tutorial de Rust sobre solicitudes HTTP](https://rust-lang-nursery.github.io/rust-cookbook/web/clients/reqwest.html)
- [Ejemplos de código de Rust sobre solicitudes HTTP](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=c1eae8f53b8bf6c04534d60c81c05998)