---
title:                "Escrevendo para o erro padrão"
aliases: - /pt/rust/writing-to-standard-error.md
date:                  2024-02-03T19:34:23.043281-07:00
model:                 gpt-4-0125-preview
simple_title:         "Escrevendo para o erro padrão"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Escrever para o erro padrão (stderr) em Rust é sobre direcionar mensagens de erro e diagnósticos para o console separadamente da saída padrão (stdout). Os programadores fazem isso para diferenciar a saída normal do programa das mensagens de erro, facilitando o tratamento adequado dos erros ou o redirecionamento deles para logs ou arquivos durante a execução.

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
