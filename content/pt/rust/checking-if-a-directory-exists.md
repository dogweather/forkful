---
title:                "Verificando se um diretório existe"
aliases:
- pt/rust/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:41.790894-07:00
model:                 gpt-4-0125-preview
simple_title:         "Verificando se um diretório existe"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?
No desenvolvimento de software, é frequentemente necessário verificar se um diretório existe para evitar erros ao tentar acessar, ler ou escrever arquivos. Rust, sendo uma linguagem de programação de sistemas, oferece métodos robustos para realizar essa tarefa, garantindo que o seu programa possa manipular arquivos e diretórios de forma segura e eficiente.

## Como:
A biblioteca padrão do Rust (`std`) inclui funcionalidades para verificar a existência de um diretório por meio dos módulos `std::path::Path` e `std::fs`. Aqui está um exemplo simples usando a abordagem padrão do Rust:

```rust
use std::path::Path;

fn main() {
    let path = Path::new("/caminho/para/diretorio");
    if path.exists() && path.is_dir() {
        println!("O diretório existe.");
    } else {
        println!("O diretório não existe.");
    }
}
```

Saída de amostra, assumindo que o diretório existe:
```
O diretório existe.
```

Para cenários mais complexos ou funcionalidades aprimoradas (como operações assíncronas no sistema de arquivos), você pode considerar o uso de uma biblioteca de terceiros, como `tokio`, com seu módulo de `fs` assíncrono, especialmente se você está trabalhando dentro de um runtime assíncrono. Veja como você poderia alcançar o mesmo com `tokio`:

Primeiro, adicione `tokio` ao seu `Cargo.toml`:

```toml
[dependencies]
tokio = { version = "1.0", features = ["full"] }
```

Depois, use `tokio::fs` para verificar se um diretório existe de forma assíncrona:

```rust
use tokio::fs;

#[tokio::main]
async fn main() {
    let path = "/caminho/para/diretorio";
    match fs::metadata(path).await {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("O diretório existe.");
            } else {
                println!("O caminho existe mas não é um diretório.");
            }
        },
        Err(_) => println!("O diretório não existe."),
    }
}
```

Saída de amostra, assumindo que o diretório não existe:
```
O diretório não existe.
```

Esses exemplos destacam como Rust e seu ecossistema oferecem abordagens tanto síncronas quanto assíncronas para verificações de existência de diretórios, atendendo a uma ampla gama de necessidades de desenvolvimento de software.
