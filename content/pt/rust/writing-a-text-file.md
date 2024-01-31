---
title:                "Escrevendo um arquivo de texto"
date:                  2024-01-19
html_title:           "Arduino: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Escrever um arquivo de texto em Rust envolve salvar dados em formato legível num arquivo no sistema de arquivos. Programadores fazem isso para persistir informação como configurações, logs ou intercâmbio de dados entre sistemas.

## Como Fazer:

```Rust
use std::fs::File;
use std::io::Write;

fn main() -> std::io::Result<()> {
    let mut arquivo = File::create("saida.txt")?;
    arquivo.write_all(b"Olá, Rustaceans!")?;
    Ok(())
}
```

_Ao executar, cria um arquivo chamado `saida.txt` com o conteúdo "Olá, Rustaceans!"._

## Mergulho Profundo

Antigamente, manipular arquivos em linguagens de baixo nível exigia conhecimento detalhado do sistema operacional. Em Rust, a biblioteca padrão abstrai esses detalhes com o módulo `std::fs`. Alternativas incluem usar bibliotecas de terceiros como `tokio` para I/O assíncrono. A efetividade vem da segurança de tipos e gestão de erros, que previne corrupção de dados e vazamentos de memória.

## Veja Também

- [Documentação oficial std::fs](https://doc.rust-lang.org/std/fs/)
- [Guia de programação de Rust](https://doc.rust-lang.org/book/)
- [Tutorial sobre I/O assíncrono com tokio](https://tokio.rs/tokio/tutorial)
