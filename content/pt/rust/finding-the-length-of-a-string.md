---
title:                "Rust: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Por Que

Encontrar o comprimento de uma string é uma tarefa comum em programação e em Rust não é diferente. Saber como fazer isso pode ser útil em uma variedade de situações, como manipulação de entrada de usuário, processamento de texto e validação de dados. Além disso, é um bom exercício para aprender as funcionalidades básicas da linguagem.

## Como Fazer

Para encontrar o comprimento de uma string em Rust, podemos usar a função `len()`. Esta função retorna o número de bytes na string, o que equivale ao seu comprimento. Veja um exemplo simples:

```Rust
let my_string = String::from("Olá mundo!");

let length = my_string.len();

println!("O comprimento da string é: {}", length);
```

Output:
```
O comprimento da string é: 10
```

Aqui, criamos uma variável `my_string` contendo a string "Olá mundo!" e, em seguida, usamos a função `len()` para encontrar seu comprimento e imprimi-lo na tela. Fácil, não é?

Mas e se quisermos encontrar o comprimento de uma string que contém caracteres especiais, como acentos ou emojis? Nesse caso, precisamos usar a função `chars().count()`, que conta o número de caracteres em uma string e não o número de bytes. Por exemplo:

```Rust
let my_string = String::from("Olá mundo! 🌎");

let length = my_string.chars().count();

println!("O comprimento da string é: {}", length);
```

Output:
```
O comprimento da string é: 10
```

Agora, o comprimento da string é 10, mesmo com o emoji adicionado. Isso porque a função `chars()` conta cada caractere individualmente, inclusive os emojis, enquanto a função `len()` conta o número de bytes na string.

## Deep Dive

Ao usar a função `len()`, é importante lembrar que ela conta o número de bytes em uma string, e não o número de caracteres ou mesmo o número de letras. Isso ocorre porque Rust trata todas as strings como UTF-8, ou seja, cada caractere possui um certo número de bytes. Por exemplo, o caractere "á" pode ser representado por dois ou três bytes em UTF-8, dependendo da forma como foi codificado.

Portanto, ao usar a função `len()`, podemos obter resultados inesperados em alguns casos. É por isso que, ao contar o número de caracteres em uma string, é melhor usar a função `chars().count()` para garantir a precisão.

## Veja Também

- [Rust Strings 101](https://www.rust-lang.org/learn/strings)
- [Standard Library: Strings](https://doc.rust-lang.org/std/string/index.html)
- [Rust by Example: Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
- [Rust String Methods](https://doc.rust-lang.org/std/string/struct.String.html#methods)