---
title:                "Rust: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Capitalize é uma função importante em muitas linguagens de programação, incluindo Rust. Ele permite que você converta uma string para que a primeira letra de cada palavra seja maiúscula. Isso é útil em muitos contextos, como na criação de títulos ou na formatação de dados.

## Como fazer

Para capitalizar uma string em Rust, você pode usar o método `to_title_case()` da biblioteca `str`, que é importado automaticamente para cada arquivo Rust. Aqui está um exemplo de como usá-lo em um programa simples:

```rust
fn main() {
    let my_string = "hello world";
    let capitalized = my_string.to_title_case();
    println!("{}", capitalized); // Saída: Hello World
}
```

Você também pode usar a macro `title_case` da biblioteca `strum`, que é útil para formatar strings em conformidade com regras de capitalização específicas, como para nomes próprios ou títulos. Aqui está um exemplo:

```rust
use strum::title_case::title_case;

fn main() {
    let my_string = "the dark knight";
    let capitalized = title_case(my_string);
    println!("{}", capitalized); // Saída: The Dark Knight
}
```

## Uma análise mais profunda

A função `to_title_case()` usa as regras do título do livro "The Chicago Manual of Style", que é amplamente utilizado na indústria editorial. Isso inclui recursos como lidar com acrônimos, palavras com hifens e pontuações especiais.

Já a macro `title_case` permite que você especifique suas próprias regras de capitalização através de um enum especializado, que pode ser útil em situações onde você precisa formatar strings de acordo com outras convenções de escrita.

Em geral, é importante lembrar que a capitalização é sensível à cultura e pode variar em diferentes idiomas. Certifique-se de considerar esses aspectos ao trabalhar com strings em suas aplicações Rust.

## Ver também

- Documentação oficial da função `to_title_case()`: https://doc.rust-lang.org/std/primitive.str.html#method.to_title_case
- Documentação oficial da macro `title_case`: https://docs.rs/strum/0.12.0/strum/title_case/index.html
- Site do "The Chicago Manual of Style": http://www.chicagomanualofstyle.org/home.html