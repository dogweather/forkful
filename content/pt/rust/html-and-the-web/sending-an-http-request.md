---
date: 2024-01-20 18:00:33.644654-07:00
description: "Enviar uma requisi\xE7\xE3o HTTP \xE9 o m\xE9todo atrav\xE9s do qual\
  \ o seu programa pode pedir dados a um servidor ou uma API na web. Programadores\
  \ fazem isso para\u2026"
lastmod: '2024-03-13T22:44:46.364591-06:00'
model: gpt-4-1106-preview
summary: "Enviar uma requisi\xE7\xE3o HTTP \xE9 o m\xE9todo atrav\xE9s do qual o seu\
  \ programa pode pedir dados a um servidor ou uma API na web."
title: "Enviando uma requisi\xE7\xE3o HTTP"
weight: 44
---

## O Que & Porquê?

Enviar uma requisição HTTP é o método através do qual o seu programa pode pedir dados a um servidor ou uma API na web. Programadores fazem isso para interagir com serviços web, buscar dados, enviar informações, ou começar processos remotos.

## Como fazer:

Para enviar uma requisição HTTP em Rust, você pode usar a crate `reqwest`, que simplifica a maioria das tarefas de networking. Primeiro, adicione a crate ao seu `Cargo.toml`:

```toml
[dependencies]
reqwest = "0.11"
tokio = { version = "1", features = ["full"] }
```

E aqui está um exemplo de como você pode usar `reqwest` para fazer uma requisição GET:

```rust
use reqwest::Error;

#[tokio::main]
async fn main() -> Result<(), Error> {
    let res = reqwest::get("http://httpbin.org/get").await?;
    
    println!("Status: {}", res.status());
    println!("Headers:\n{:#?}", res.headers());
    
    let body = res.text().await?;
    println!("Body:\n{}", body);
    
    Ok(())
}
```

Saída de exemplo:

```
Status: 200 OK
Headers:
{
    "content-type": "application/json",
    ...
}
Body:
{
  "args": {},
  "headers": {
    "Host": "httpbin.org",
    ...
  },
  "origin": "..."
}
```

## Mergulho Profundo:

A declaração de `#[tokio::main]` prepara o cenário para o uso de `async-await`, que é essencial em operações que podem bloquear, como solicitações de rede. Historicamente, alternativas como `hyper` exigiam mais configuração e detalhes da implementação, mas `reqwest` abstrai isso, oferecendo uma interface simples. A biblioteca passou por várias iterações e melhorias de desempenho ao longo dos anos, o que reflete a evolução do async Rust.

Falando em alternativas, além de `reqwest`, há outras crates que você pode explorar, como `hyper` (para quem precisa de controle detalhado) e `surf` (uma opção mais recente que só suporta async).

Quanto a implementação, é importante entender como `reqwest` lida com assincronia, que permite ao Rust realizar outras tarefas enquanto espera a resposta de uma requisição. Isso é feito por meio de um runtime `async`, geralmente fornecido pelo `tokio`. Sem ele, você teria de lidar com muita complexidade de baixo nível de I/O e concorrência.

## Veja Também:

Para se aprofundar mais na biblioteca `reqwest` e suas capacidades:
- Documentação da crate `reqwest`: https://docs.rs/reqwest/

Para entender os conceitos de `async-await` no Rust:
- Rust book sobre Concorrência Assíncrona: https://doc.rust-lang.org/book/ch16-05-async.html

Para uma visão geral do ecossistema de solicitações de rede em Rust, incluindo o `hyper` e `surf`:
- Uma comparação entre `hyper`, `reqwest`, e `surf`: https://www.arewewebyet.org/topics/libraries/#http-clients
