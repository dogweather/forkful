---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:23.043281-07:00
description: "Como fazer: Rust oferece uma maneira simples de escrever para stderr\
  \ usando a macro `eprintln!`, semelhante a como `println!` \xE9 usada para stdout.\
  \ Aqui\u2026"
lastmod: '2024-03-13T22:44:46.384028-06:00'
model: gpt-4-0125-preview
summary: "Rust oferece uma maneira simples de escrever para stderr usando a macro\
  \ `eprintln!`, semelhante a como `println!` \xE9 usada para stdout."
title: "Escrevendo para o erro padr\xE3o"
weight: 25
---

## Como fazer:
Rust oferece uma maneira simples de escrever para stderr usando a macro `eprintln!`, semelhante a como `println!` é usada para stdout. Aqui está um exemplo básico:

```rust
fn main() {
    eprintln!("Esta é uma mensagem de erro!");
}
```

Saída de amostra (para erro padrão):
```
Esta é uma mensagem de erro!
```

Para mais controle sobre as mensagens de erro, como quando você deseja formatar texto ou tratar resultados de I/O, use a função `stderr` do módulo `std::io`. Este método fornece um manipulador para o fluxo global de stderr, que você pode então escrever usando métodos como `write_all` ou `writeln` do trait `Write`:

```rust
use std::io::{self, Write};

fn main() {
    let stderr = io::stderr();
    let mut handle = stderr.lock();
    
    writeln!(handle, "Mensagem de erro formatada: {}", 404).expect("Falha ao escrever no stderr");
}
```

Saída de amostra (para erro padrão):
```
Mensagem de erro formatada: 404
```

Se você está trabalhando em ambientes ou aplicações onde depende de bibliotecas para registro de logs ou tratamento de erros, bibliotecas como `log` e `env_logger` são populares. Embora sejam mais usadas para fins de registro de logs, elas são configuráveis e podem direcionar níveis de log de erro para stderr. Abaixo está um exemplo simples de uso do `log` e `env_logger`:

Primeiro, adicione as dependências ao seu `Cargo.toml`:
```toml
[dependencies]
log = "0.4"
env_logger = "0.9"
```

Em seguida, configure e use o registro de logs na sua aplicação:
```rust
fn main() {
    env_logger::init();
    log::error!("Esta é uma mensagem de erro registrado no stderr");
}
```

Executando este programa (após configurar o `env_logger` com uma variável de ambiente apropriada, por exemplo, `RUST_LOG=error`) irá sair a mensagem de erro para stderr, utilizando a infraestrutura de registro de logs.

```plaintext
ERROR: Esta é uma mensagem de erro registrado no stderr
```
