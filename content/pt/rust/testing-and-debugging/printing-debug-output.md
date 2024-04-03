---
date: 2024-01-20 17:53:20.529372-07:00
description: "Imprimir sa\xEDdas de debug \xE9 simplesmente mostrar na tela o estado\
  \ de vari\xE1veis ou o fluxo do c\xF3digo. Programadores fazem isso para entender\
  \ e corrigir\u2026"
lastmod: '2024-03-13T22:44:46.370232-06:00'
model: gpt-4-1106-preview
summary: "Imprimir sa\xEDdas de debug \xE9 simplesmente mostrar na tela o estado de\
  \ vari\xE1veis ou o fluxo do c\xF3digo."
title: "Exibindo sa\xEDdas de depura\xE7\xE3o"
weight: 33
---

## How to:
Rust tem macros legais pra isso: `println!` para exibir textos e `dbg!` para detalhes de debug.

```Rust
fn main() {
    let numero = 42;
    println!("O número é: {}", numero); // Exibe uma mensagem simples.
    dbg!(numero); // Exibe informações detalhadas de debug, incluindo o local no código.
}
```

Output esperado:
```
O número é: 42
[src/main.rs:4] numero = 42
```

## Deep Dive
Antes de `dbg!`, Rustaceans usavam muitos `println!` com formatos especiais para debug. Mas `dbg!` veio pra simplificar: ela retorna o valor passado, então você pode inserir no meio do código sem problemas.

Existem outras ferramentas, como logging por níveis com `log` e `env_logger`, que são mais flexíveis, mas também mais complexas.

Detalhe: `dbg!` imprime no stderr, não no stdout; importante pro caso de redirecionar saídas.

## See Also
- Documentação do `std::fmt` (formatação): https://doc.rust-lang.org/std/fmt/
- Crates de logging: https://crates.io/categories/logging
- Debugging com GDB: https://forge.rust-lang.org/debugging.html
