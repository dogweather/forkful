---
title:    "Rust: Maiúsculas em uma string"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Por que capitalizar uma string em Rust?

Em programação, é comum precisar modificar strings de texto para torná-las mais legíveis ou formatá-las para determinados propósitos. Em Rust, uma das formas de fazer isso é através da capitalização de strings. Neste artigo, explicaremos o porquê disso ser importante e como podemos fazer isso em Rust.

## Como fazer em Rust

Em Rust, podemos capitalizar uma string de duas maneiras: utilizando o método `to_uppercase()` ou `to_lowercase()`, ambos pertencentes à classe `String`. Vejamos um exemplo de como utilizá-los:

```Rust
let minha_string = "exemplo de string";
let string_capitalizada = minha_string.to_uppercase();
println!("{}", string_capitalizada);
// Output: "EXEMPLO DE STRING"

let minha_string = "exemplo de string";
let string_capitalizada = minha_string.to_lowercase();
println!("{}", string_capitalizada);
// Output: "exemplo de string"
```

Também é possível utilizar esses métodos juntamente com a função `format!()` para capitalizar apenas a primeira letra da string, por exemplo:

```Rust
let minha_string = "exemplo de string";
let primeira_letra = minha_string[0..1].to_uppercase();
let resto_da_string = minha_string[1..].to_lowercase();
let string_final = format!("{}{}", primeira_letra, resto_da_string);
println!("{}", string_final);
// Output: "Exemplo de string"
```

## Profundidade na capitalização de strings

Quando utilizamos os métodos `to_uppercase()` e `to_lowercase()`, é importante lembrar que eles retornam uma nova string, não modificando a original. Além disso, esses métodos seguem as regras de capitalização do sistema operacional em que o código está sendo executado. Portanto, se o sistema operacional estiver em inglês, as letras maiúsculas serão diferentes do que se estivesse em português, por exemplo.

Outra maneira de capitalizar strings em Rust é utilizando a biblioteca `Inflector`, que tem suporte para diferentes idiomas e pode ser encontrada no [crates.io](https://crates.io/crates/inflector). Com essa biblioteca, podemos usar o método `to_title_case()` que é mais preciso ao seguir as regras de capitalização da língua específica.

## Veja também

- [Documentação oficial do Rust sobre strings](https://doc.rust-lang.org/std/string/struct.String.html)
- [Biblioteca Inflector no crates.io](https://crates.io/crates/inflector)
- [Exemplos de uso do Inflector no Github](https://github.com/whatisinternet/inflections-rs/blob/master/doc/Inflector.md)