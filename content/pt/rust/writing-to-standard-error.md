---
title:    "Rust: Escrevendo no erro padrão"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é importante para identificar e solucionar problemas em seu código. Isso permite que você veja a saída de erros do seu programa, o que pode ajudá-lo a detectar e corrigir erros em tempo de execução.

## Como fazer

Escrever para o erro padrão é simples em Rust. Basta usar a macro `eprintln!`, seguida pelo que você deseja imprimir. Por exemplo:

```Rust
eprintln!("Erro encontrado: {}", error);
```

Isso imprimirá o erro no console como uma mensagem de erro padrão, facilitando a identificação e correção de problemas.

Aqui está um exemplo completo:

```Rust
fn main() {
    let num1 = 10;
    let num2 = 0;

    let result = num1 / num2;

    eprintln!("Resultado: {}", result);
}
```

A saída para este código será `Erro encontrado: thread 'main' panicked at 'attempt to divide by zero`, indicando que o programa tentou dividir um número por zero, o que é impossível. Com a mensagem de erro, você pode facilmente identificar e corrigir o problema.

## Mergulho profundo

Ao escrever para o erro padrão em Rust, é importante entender que há duas principais formas de saída de erro: `println!` e `eprintln!`. Enquanto `println!` imprime para a saída padrão, `eprintln!` imprime para o erro padrão. Isso significa que, ao usar `eprintln!`, suas mensagens de erro serão impressas em vermelho, facilitando a identificação.

Também é importante notar que tanto `println!` quanto `eprintln!` internamente chamam a função `format!`, que formata as strings antes de imprimi-las. Isso significa que você também pode usar formatação de strings em suas mensagens de erro, como por exemplo:

```Rust
eprintln!("Erro encontrado: {} na linha {}", error, line_number);
```

Isso imprimirá uma mensagem de erro com mais informações úteis para solucionar o problema.

## Veja também

- [Documentação oficial do Rust sobre saída de erro](https://doc.rust-lang.org/std/macro.eprintln.html)
- [Artigo sobre como escrever para o erro padrão em Rust](https://ravenzz.github.io/writing-to-std-err-with-eprintln/)