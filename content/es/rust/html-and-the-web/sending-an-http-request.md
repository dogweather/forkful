---
date: 2024-01-20 18:00:34.805176-07:00
description: "C\xF3mo hacerlo: Para enviar una petici\xF3n HTTP en Rust, es com\xFA\
  n usar la librer\xEDa `reqwest`, la cual permite manejar peticiones de forma sencilla.\
  \ Aqu\xED un\u2026"
lastmod: '2024-03-13T22:44:58.843325-06:00'
model: gpt-4-1106-preview
summary: "Para enviar una petici\xF3n HTTP en Rust, es com\xFAn usar la librer\xED\
  a `reqwest`, la cual permite manejar peticiones de forma sencilla."
title: Enviando una solicitud http
weight: 44
---

## Cómo hacerlo:
Para enviar una petición HTTP en Rust, es común usar la librería `reqwest`, la cual permite manejar peticiones de forma sencilla. Aquí un ejemplo:

```Rust
use reqwest;

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let respuesta = reqwest::get("https://httpbin.org/ip").await?;
    let cuerpo = respuesta.text().await?;

    println!("Cuerpo de la respuesta: {}", cuerpo);
    Ok(())
}
```

Salida de muestra:

```
Cuerpo de la respuesta: {
    "origin": "123.45.67.89"
}
```

## Deep Dive:
El envío de peticiones HTTP no siempre fue tan sencillo en Rust. Antes de `reqwest`, usar `hyper` directamente era más común, pero era bajo nivel y complicado. `reqwest` se basa en `hyper`, proporcionando una interfaz más amigable.

Alternativas a `reqwest` incluyen:

- `ureq`: para casos de uso síncronos.
- `surf`: una opción asíncrona y ligera.

Detalles de implementación clave para `reqwest` incluyen:

1. Manejo de tareas asíncronas, para lo cual Rust usa el modelo de `async/await`.
2. Certificados TLS para peticiones seguras, gestionados automáticamente.
3. Soporte para diferentes métodos HTTP (GET, POST, etc.) y tipos de cuerpo (texto, JSON, formularios).

## Ver También:
- [Reqwest - Documentación oficial](https://docs.rs/reqwest/)
- [Rust Asynchronous Programming - Libro Oficial](https://rust-lang.github.io/async-book/)
- [HTTP - Descripción del protocolo](https://developer.mozilla.org/es/docs/Web/HTTP)

Esta información desarrolla una base para trabajar con peticiones HTTP en Rust. La práctica y la exploración de la documentación y recursos ampliarán tu habilidad en esta área esencial de la programación para la web.
