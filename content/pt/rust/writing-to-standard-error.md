---
title:                "Escrevendo no erro padrão"
date:                  2024-01-19
simple_title:         "Escrevendo no erro padrão"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## O Que É & Por Quê?
Escrever no erro padrão (stderr) é a forma de enviar mensagens de erro e diagnósticos em um programa. Programadores fazem isso para separar a saída normal (stdout) da saída de erro, facilitando a identificação e manipulação de problemas.

## Como Fazer:
```Rust
use std::io::{self, Write};

fn main() {
    writeln!(io::stderr(), "Erro encontrado!").expect("Falha ao escrever no stderr");
}
```
Saída esperada na stderr:
```
Erro encontrado!
```

## Mergulho Profundo
Historicamente, a separação entre stdout e stderr permite que erros sejam redirecionados ou tratados de forma diferente da saída padrão de um programa. Alternativas incluem o uso de bibliotecas de logging para maior controle. A implementação em Rust é facilitada pelo módulo `std::io`, que oferece funções diretas para escrever no stderr.

## Veja Também
- A documentação oficial do Rust sobre `std::io`: https://doc.rust-lang.org/std/io/
- Um tutorial sobre manipulação de erros em Rust: https://doc.rust-lang.org/book/ch09-00-error-handling.html
- Um guia sobre o uso de stderr em diferentes linguagens de programação: https://en.wikipedia.org/wiki/Standard_streams#Standard_error_(stderr)
