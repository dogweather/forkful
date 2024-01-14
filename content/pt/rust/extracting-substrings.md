---
title:    "Rust: Extraindo subcadeias"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que usar a extração de substrings em Rust?

A extração de substrings é uma técnica útil para obter partes específicas de uma string. Isso pode ser útil em uma variedade de situações, como formatar dados, validar entradas do usuário, manipular texto e muito mais. Em Rust, essa tarefa pode ser facilmente realizada utilizando o método `get()` da classe `str`. Neste post, vamos explorar como realizar a extração de substrings em Rust.

## Como fazer a extração de substrings em Rust

Para extrair um substring em Rust, primeiro devemos criar uma variável contendo a string original. Em seguida, usamos a função `get()` para obter a parte desejada da string. Vamos ver um exemplo de como extrair os três primeiros caracteres de uma string:

```Rust
let string = "Olá, mundo!";
let substr = string.get(0..3); // extrai os caracteres "Olá"
println!("{}", substr); // imprime "Olá"
```

Observe que o índice inicial começa em 0 e o final é exclusivo, ou seja, os caracteres na posição 0, 1 e 2 (totalizando 3 caracteres) serão extraídos.

Podemos também utilizar a função `chars()` para separar uma string em um vetor de caracteres e, em seguida, usar o método `get()` para obter apenas uma parte desejada dessa string. Por exemplo:

```Rust
let string = "Olá, mundo!";
let chars = string.chars().collect::<Vec<char>>();
let substr = chars.get(5..9); // extrai a palavra "mundo"
println!("{:?}", substr); // imprime ["m", "u", "n", "d"]
```

## Detalhando a extração de substrings em Rust

Ao utilizar o método `get()` em uma string, é importante entender como os índices são contados. Os índices em Rust começam em 0 e vão até o tamanho da string - 1. Para extrair os últimos caracteres de uma string, podemos utilizar valores negativos nos índices. Por exemplo:

```Rust
let string = "Olá, mundo!";
let substr = string.get(-6..); // extrai os últimos 6 caracteres
println!("{}", substr); // imprime "mundo!"
```

Além disso, também podemos usar a função `len()` para obter o tamanho da string e assim definir os índices finais de forma dinâmica. Por exemplo, se queremos extrair os últimos 2 caracteres de uma string, podemos fazer:

```Rust
let string = "Olá, mundo!";
let size = string.len(); // obtém o tamanho da string (12)
let substr = string.get(size-2..); // extrai os últimos 2 caracteres
println!("{}", substr); // imprime "do"
```

Outra função útil para a extração de substrings é a `split()` que nos permite separar uma string em partes, com base em um caractere específico. Vamos ver um exemplo:

```Rust
let string = "Maçã;Banana;Melão;Uva";
let substr = string.split(";").collect::<Vec<&str>>(); // separa a string em partes
println!("{:?}", substr); // imprime ["Maçã", "Banana", "Melão", "Uva"]
```

## Veja também

- [Documentação oficial do método get()](https://doc.rust-lang.org/std/primitive.str.html#method.get)
- [Tutorial de Rust para iniciantes](https://www.rust-lang.org/learn/get-started)
- [Exemplos de codificação em Rust](https://doc.rust-lang.org/stable/rust-by-example/index.html#strings)