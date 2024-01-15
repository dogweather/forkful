---
title:                "Escrevendo no erro padrão"
html_title:           "Rust: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever no erro padrão?

Escrever no erro padrão é uma prática comum em muitas linguagens de programação, incluindo Rust. Isso é feito para informar sobre possíveis erros ou exceções que podem ocorrer durante a execução do programa. Além disso, também pode ser útil para depuração e registro de informações importantes.

## Como Fazer

Para escrever no erro padrão em Rust, podemos usar a função `eprintln!()` que é fornecida pela biblioteca padrão da linguagem. Esta função funciona de maneira semelhante à `println!()` usada para imprimir na saída padrão, mas ao invés de imprimir na tela, ela imprime no erro padrão.

Um exemplo simples de código que usa `eprintln!()` pode ser o seguinte:

```rust
fn main() {

    let num = 10;
    let den = 0;

    if den == 0 {
        // Usando a função eprintln!() para informar sobre o erro
        eprintln!("Não é possível dividir por zero!");
        // Finalizando o programa
        return;
    }

    let resultado = num / den;

    println!("O resultado é: {}", resultado);

}
```

Saída do programa acima:

```
Não é possível dividir por zero!
```

Note que a mensagem de erro foi impressa usando `eprintln!()`.

## Mergulho Profundo

A função `eprintln!()` é uma macro, que em Rust é como uma função, mas com algumas diferenças. Uma dessas diferenças é que ela pode receber uma expressão que será avaliada e convertida em uma string para ser impressa no erro padrão.

Por exemplo, podemos usar a sintaxe `eprintln!("Valor de x: {}", x)` para imprimir o valor da variável `x` no erro padrão. Além disso, é possível formatar a saída usando as mesmas especificações de formatação usadas nas macros `println!()` e `format!()`.

## Veja Também

- [Documentação oficial do Rust sobre escrever no erro padrão](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Tutorial sobre manipulação de erros em Rust](https://blog.logrocket.com/error-handling-in-rust/)