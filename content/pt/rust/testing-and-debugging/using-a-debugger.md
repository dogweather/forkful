---
aliases:
- /pt/rust/using-a-debugger/
date: 2024-01-26 04:10:20.053395-07:00
description: "Usar um depurador \xE9 como dar a si mesmo uma vis\xE3o de raio-x para\
  \ espiar a execu\xE7\xE3o do seu c\xF3digo. Programadores fazem isso para identificar\
  \ bugs, entender\u2026"
lastmod: 2024-02-18 23:08:57.933354
model: gpt-4-0125-preview
summary: "Usar um depurador \xE9 como dar a si mesmo uma vis\xE3o de raio-x para espiar\
  \ a execu\xE7\xE3o do seu c\xF3digo. Programadores fazem isso para identificar bugs,\
  \ entender\u2026"
title: Usando um depurador
---

{{< edit_this_page >}}

## O Que & Por Quê?

Usar um depurador é como dar a si mesmo uma visão de raio-x para espiar a execução do seu código. Programadores fazem isso para identificar bugs, entender o fluxo do programa e garantir que seu código esteja limpo como um apito. É como ter um amigo que aponta exatamente onde você tropeçou.

## Como:

Rust suporta vários depuradores, mas um comum é o `gdb` para GNU/Linux ou `lldb` para macOS. Você também pode usar `rust-gdb` ou `rust-lldb`, que são wrappers que imprimem de forma bonita os valores Rust. Aqui está uma olhada:

```Rust
fn main() {
    let mut counter = 0;
    for _ in 0..5 {
        counter += 1;
        println!("Counter is at: {}", counter);
    }
}
```

Para depurar isso, compile com informações de depuração:

```shell
$ rustc -g counter.rs
```

Então execute no `rust-gdb`:

```shell
$ rust-gdb counter
(gdb) break main
(gdb) run
(gdb) print counter
$1 = 0
(gdb) continue
Counter is at: 1
(gdb) print counter
$2 = 1
```

## Mergulho Profundo

A depuração existe desde os *tempos antigos* dos cartões perfurados, e sua evolução tem sido uma dádiva. Rust oferece sua própria ferramentaria com integrações para GDB e LLDB devido à natureza de nível de sistema da linguagem.

Alternativas para depurar código Rust incluem o uso de ambientes de desenvolvimento integrados (IDEs) com seus depuradores embutidos, que alguns acham mais intuitivos. Os populares incluem o CLion com o plugin Rust ou o Visual Studio Code com a extensão Rust.

Quanto à implementação, Rust gera símbolos de depuração que esses depuradores entendem, o que é vital para avançar pelo código, definir pontos de interrupção e inspecionar variáveis sem perder a sanidade.

## Veja Também

- O Livro Rust sobre Depuração: https://doc.rust-lang.org/book/ch09-02-recoverable-errors-with-result.html#guidelines-for-error-handling
- A abordagem de Rust Por Exemplo sobre Erros e Depuração: https://doc.rust-lang.org/rust-by-example/error.html
- O Servidor de Linguagem Rust (RLS), que alimenta a extensão Rust do VS Code: https://github.com/rust-lang/rls
- Depurando Rust com Visual Studio Code: https://marketplace.visualstudio.com/items?itemName=rust-lang.rust
