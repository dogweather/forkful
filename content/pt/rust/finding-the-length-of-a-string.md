---
title:    "Rust: Encontrando o comprimento de uma string"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por que?

Encontrar o comprimento de uma string é uma tarefa comum em programação, especialmente quando se trabalha com entradas de usuário ou manipulação de texto. Em Rust, a linguagem de programação de sistema moderna, essa tarefa é feita de forma eficiente e segura, graças às suas características únicas.

## Como fazer

Para encontrar o comprimento de uma string em Rust, podemos usar o método `len` da estrutura `String`. Este método retorna o número de bytes presentes na string, o que pode ser diferente do número de caracteres dependendo dos caracteres utilizados.

Vamos dar uma olhada em um exemplo para ilustrar isso:

```Rust
let minha_string = String::from("Olá, mundo!");

println!("Minha string tem o comprimento de {} bytes.", minha_string.len());
```

A saída deste código será "Minha string tem o comprimento de 13 bytes.", uma vez que a string contém 13 caracteres, mas 15 bytes. Isso ocorre porque os caracteres acentuados em UTF-8 ocupam mais de 1 byte.

## Dando um mergulho mais profundo

Uma coisa interessante sobre o método `len` é que ele também pode ser usado para iterar em uma string. Por exemplo:

```Rust
let minha_string = String::from("Olá, mundo!");

for i in 0..minha_string.len() {
    println!("Índice: {}, Caractere: {}", i, minha_string.chars().nth(i).unwrap());
}
```

Este código irá imprimir todos os caracteres da string, juntamente com o seu índice.

É importante notar que o `len` só pode ser usado com strings em Rust se elas forem do tipo `String` e não do tipo `&str`. Isso ocorre porque `&str` não possui um tamanho conhecido em tempo de compilação.

## Veja também

- Documentação oficial do método `len` em Rust: https://doc.rust-lang.org/std/string/struct.String.html#method.len
- Um guia completo para strings em Rust: https://doc.rust-lang.org/book/ch08-02-strings.html
- Mais sobre UTF-8 e como ele funciona em Rust: https://doc.rust-lang.org/std/string/struct.String.html#examples