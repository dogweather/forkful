---
title:    "Rust: Concatenando strings"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que concatenar strings em Rust?

Concatenar strings é uma habilidade importante em qualquer linguagem de programação, incluindo Rust. Isso permite que você combine múltiplas strings em uma só, tornando mais fácil a manipulação e exibição de informações para o usuário final. Além disso, concatenar strings também pode ser útil para lidar com entrada de dados e formatação de texto.

## Como fazer em Rust

Em Rust, podemos utilizar a macro `format!` para concatenar strings de forma eficiente. Por exemplo:

```Rust
let nome = "João";
let sobrenome = "Silva";
let nome_completo = format!("{} {}", nome, sobrenome);
println!("Seu nome completo é: {}", nome_completo);
```

O código acima irá imprimir: `Seu nome completo é: João Silva`. Note que utilizamos `{}` para indicar onde as variáveis devem ser inseridas dentro da string.

Além disso, também é possível utilizar o operador `+` para concatenar strings, porém essa abordagem é menos eficiente. Veja um exemplo:

```Rust
let mensagem = "Olá " + "mundo!";
```

## Mergulho Profundo

Ao utilizar a macro `format!`, podemos realizar a formatação de strings de diferentes formas. Por exemplo:

```Rust
let valor = 10;
let mensagem = format!("O dobro de {} é {}", valor, valor * 2);
println!(mensagem); // Irá imprimir: O dobro de 10 é 20
```

Também é possível adicionar textos ou símbolos entre as strings concatenadas:

```Rust
let mensagem = format!("10 + 5 = {}", 10 + 5);
println!(mensagem); // Irá imprimir: 10 + 5 = 15
```

E se precisarmos inserir valores que não sejam strings em uma string formatada, podemos utilizar a sintaxe `{:?}`:

```Rust
let valores = vec![1, 2, 3];
println!("O vetor possui {:?} elementos.", valores); // Irá imprimir: O vetor possui [1, 2, 3] elementos.
```

## Veja também

- [Documentação sobre strings em Rust](https://doc.rust-lang.org/std/string/index.html)
- [Tutorial sobre formatação de strings em Rust](https://www.geeksforgeeks.org/formatting-output-in-rust-programming-language/)