---
title:                "Rust: Capitalizando uma String"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que capitalizar uma string?

Capitalizar uma string pode ser útil em diversas situações. Por exemplo, quando estamos trabalhando com nomes próprios ou títulos, é importante que a primeira letra esteja em maiúscula. Além disso, capitalizar uma string pode ser um requisito para o funcionamento de alguns algoritmos ou APIs.

## Como fazer?

Em Rust, podemos capitalizar uma string usando o método `to_uppercase()`. Veja um exemplo abaixo:

```Rust
let string = "olá mundo";
let string_capitalizada = string.to_uppercase();
println!("String original: {}", string);
println!("String capitalizada: {}", string_capitalizada);
```

A saída deste código seria:

```
String original: olá mundo
String capitalizada: OLÁ MUNDO
```

## Aprofundando-se

Ao usar o método `to_uppercase()`, é importante ter em mente que ele irá capitalizar todas as letras da string, incluindo as já maiúsculas. Para evitar isso, podemos usar o método `to_lowercase()` primeiro.

Outro ponto interessante é que o método `to_uppercase()` só funciona corretamente com caracteres ASCII. Se sua string contiver caracteres Unicode, é necessário usar o método `chars()` para iterar sobre cada caractere e aplicar a capitalização individualmente.

## Veja também

Para mais informações sobre como trabalhar com strings em Rust, confira a documentação da linguagem:

- [Documentação do tipo String em Rust](https://doc.rust-lang.org/std/string/struct.String.html)
- [Tutorial sobre strings em Rust](https://doc.rust-lang.org/book/ch08-02-strings.html)

Se você quer aprender mais sobre Rust, confira estes recursos:

- [Rust Brasil - Comunidade de desenvolvedores brasileiros em Rust](https://rust-br.org/)
- [Rust by Example - Uma coleção de exemplos práticos em Rust](https://doc.rust-lang.org/rust-by-example/index.html)
- [The Rust Programming Language - Livro oficial sobre Rust](https://doc.rust-lang.org/book/)