---
title:                "Imprimindo a saída de depuração"
html_title:           "Rust: Imprimindo a saída de depuração"
simple_title:         "Imprimindo a saída de depuração"
programming_language: "Rust"
category:             "Rust"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saídas de depuração?

Se você já se viu tentando descobrir por que seu programa não está funcionando corretamente, provavelmente já desejou que pudesse olhar dentro do seu código e ver o que está acontecendo em cada etapa. A boa notícia é que podemos fazer isso com a impressão de saídas de depuração, o que nos permite ver o valor das variáveis e as informações importantes em cada etapa da execução do nosso programa.

## Como fazer

Para imprimir saídas de depuração em Rust, podemos usar o macro `println!`, que funciona de forma semelhante ao `printf` em outras linguagens. Vamos dar uma olhada em um exemplo simples:

```rust
fn main() {
    let number = 5;
    println!("O número é: {}", number);
}
```

Nesse código, criamos uma variável `number` com o valor 5 e a imprimimos usando o `println!` com o marcador de posição `{}` para indicar que queremos imprimir o valor da variável. Quando o programa for executado, veremos a seguinte saída:

```
O número é: 5
```

Podemos até mesmo imprimir múltiplas variáveis ou valores em uma única linha, adicionando mais marcadores de posição e passando os valores correspondentes depois da string de formatação. Veja um exemplo:

```rust
let x = "Olá";
let y = "mundo";
println!("{} {}, tudo bem?", x, y);
```

E a saída será:

```
Olá mundo, tudo bem?
```

## Profundando na impressão de saídas de depuração

Além do `println!`, existem outros macros úteis para imprimir saídas de depuração em Rust, como o `dbg!` e o `eprintln!`. Além disso, podemos usar formatação de strings para imprimir valores em formatos específicos, como números binários ou hexadecimais.

Também é possível adicionar informações extras nas saídas de depuração, como o nome da função que está sendo executada ou a linha em que a saída foi impressa. Isso pode ser útil para identificar onde exatamente o programa está executando em caso de erros ou comportamentos inesperados.

## Veja também

- Documentação oficial sobre saídas de depuração em Rust: https://doc.rust-lang.org/std/macro.dbg.html
- Tutorial sobre impressão de saídas de depuração em Rust: https://learning-rust.github.io/docs/e3.printing_to_stdout.html
- Exemplo prático de uso de saídas de depuração em um programa Rust: https://stevedonovan.github.io/rust-gentle-intro/5-io.html