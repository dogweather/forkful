---
title:                "Enviando una solicitud http"
html_title:           "Bash: Enviando una solicitud http"
simple_title:         "Enviando una solicitud http"
programming_language: "Rust"
category:             "Rust"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/rust/sending-an-http-request.md"
---

{{< edit_this_page >}}

## ¿Qué es & Por qué?

Enviar una solicitud HTTP en programación es como pedir información a un servidor. Los programadores hacen esto para interactuar con API's web, recuperar datos, o manipularlos.

## ¿Cómo se hace?

Para realizar solicitudes HTTP en Rust, usaremos el paquete reqwest. Incluye funcionalidades para tanto solicitudes síncronas como asíncronas.

```Rust
use reqwest;

fn main() -> Result<(), Box<dyn std::error::Error>>{
    let body = reqwest::blocking::get("http://httpbin.org/ip")?
        .text()?;

    println!("body = {:?}", body);

    Ok(())
}
```
Esto debería imprimir la IP pública como respuesta del servidor:

```
body = "{\n  \"origin\": \"123.45.67.89\"\n}\n"
```

## Análisis En Profundidad

Las técnicas para enviar solicitudes HTTP evolucionaron a lo largo de los años. Rust ofrece una forma segura y eficiente para manejar las comunicaciones HTTP. Si bien hay alternativas como hyper o isahc, reqwest es un paquete muy fácil de usar y comprender.

En cuanto a los detalles de implementación, hay que recordar siempre cerrar las conexiones HTTP, de lo contrario se pueden agotar los recursos del sistema.

## También Vea

Para una guía más completa de reqwest, visite [la documentación oficial de Reqwest](https://docs.rs/reqwest). Si necesita más detalles sobre las solicitudes HTTP en general, [MDN Web Docs es un buen recurso](https://developer.mozilla.org/es/docs/Web/HTTP).