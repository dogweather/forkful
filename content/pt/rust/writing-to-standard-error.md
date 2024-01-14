---
title:                "Rust: Escrevendo para o erro padrão"
simple_title:         "Escrevendo para o erro padrão"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma prática comum no desenvolvimento de software, especialmente em linguagens de programação como Rust. Ao direcionar a saída do programa para o erro padrão, é possível identificar e corrigir possíveis erros e bugs de forma mais eficiente. Além disso, essa abordagem pode ser útil para fins de depuração e testes.

## Como fazer

Para escrever para o erro padrão em Rust, basta usar a macro ```eprintln!``` seguida da mensagem que você deseja exibir. Por exemplo:

```Rust
eprintln!("Erro: Valor inválido!");
```

Isso irá imprimir a mensagem "Erro: Valor inválido!" no terminal.

Outra opção é usar a função ```write!```, que permite escrever para qualquer fluxo de saída, incluindo o erro padrão. Por exemplo:

```Rust
use std::io::Write;

fn main() {
    let mut err = std::io::stderr();
    write!(err, "Erro: Valor inválido!");
}
```

No entanto, a opção mais comumente utilizada é a primeira, com a macro ```eprintln!```.

## Mergulho Profundo

Ao usar a macro ```eprintln!```, a mensagem será formatada antes de ser exibida no terminal. Isso significa que você pode passar argumentos para a macro e utilizar formatação de strings, como em ```println!```. Por exemplo:

```Rust
let num = 42;
eprintln!("O número {} é o significado da vida!", num);
```

Além disso, também é possível direcionar a saída de erro para uma variável, usando a função ```format!```. Por exemplo:

```Rust
let mut err = String::new();
eprintln!(err, "Erro: {} não é um valor válido!", num);
```

## Veja também

- Documentação oficial do Rust para a macro ```eprintln!```: https://doc.rust-lang.org/std/macro.eprintln.html
- Blog post sobre como usar macros em Rust: https://blog.rust-lang.org/2017/11/14/Rust-1.22.html#eprintln-and-elog
- Vídeo tutorial sobre como escrever para o erro padrão em Rust: https://www.youtube.com/watch?v=juBoiohU9p0