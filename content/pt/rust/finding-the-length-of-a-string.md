---
title:    "Rust: Encontrando o comprimento de uma string"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que

Encontrar o comprimento de uma string é uma tarefa comum em programação, especialmente quando se lida com entrada de usuário ou processamento de dados. Saber como realizar essa tarefa em Rust é essencial para programadores que desejam dominar a linguagem.

## Como fazer

Encontrar o comprimento de uma string em Rust é simples e pode ser feito de várias maneiras. A forma mais simples é usando o método `len()` da struct `String`. Veja um exemplo abaixo:

```Rust
let string = String::from("Rust é incrível!"); // Cria uma string
let length = string.len(); // Chama o método "len()" para encontrar o comprimento
println!("O comprimento da string é: {}", length); // Imprime o resultado
```

O código acima irá retornar o comprimento da string, que é 18. Outra forma de encontrar o comprimento é usando a função `len()` da biblioteca padrão. Veja um exemplo abaixo:

```Rust
let string = "Rust é incrível!"; // Cria uma string
let length = string.len(); // Chama a função "len()" para encontrar o comprimento
println!("O comprimento da string é: {}", length); // Imprime o resultado
```

Novamente, o resultado será 18. Além disso, também é possível encontrar o comprimento de uma string em bytes usando o método `as_bytes()` da struct `String`. Veja um exemplo abaixo:

```Rust
let string = String::from("Rust é incrível!"); // Cria uma string
let bytes = string.as_bytes(); // Converte a string em um vetor de bytes
let length = bytes.len(); // Usa o método "len()" para encontrar o comprimento
println!("O comprimento da string em bytes é: {}", length); // Imprime o resultado
```

## Deep Dive

Ao lidar com strings em Rust, é importante entender que a linguagem possui dois tipos básicos de string: a `String` e a `&str`. A diferença entre eles é que a `String` é uma string em propriedade, ou seja, a linguagem é responsável por alocar e liberar a memória necessária para a string, enquanto a `&str` é um tipo de referência que aponta para uma string existente em algum lugar do código.

Quando se trata de encontrar o comprimento de uma string, ambas as formas mencionadas anteriormente (usando `len()` ou `as_bytes()`) funcionam para as duas variantes. Entretanto, vale ressaltar que o método `len()` da `&str` é mais eficiente do que o da `String`, pois a `&str` já possui o seu comprimento armazenado em sua struct.

## Ver também

- [Documentação da struct `String` em Rust](https://doc.rust-lang.org/std/string/struct.String.html)
- [Tutorial oficial de Rust sobre strings](https://doc.rust-lang.org/book/ch08-02-strings.html)
- [Vídeo explicativo sobre strings em Rust](https://www.youtube.com/watch?v=hwciPnhT9hw)