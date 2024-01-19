---
title:                "Capitalizando uma string"
html_title:           "Rust: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Rust: Como Capitalizar uma String

## O Que & Porquê?

Capitalizar uma string significa transformar a primeira letra de cada palavra em maiúscula. Programadores fazem isso para melhorar a estética e legibilidade de textos em suas aplicações.

## Como fazer:

Em Rust, você pode capitalizar uma string utilizando uma combinação de métodos do tipo `str`. Segue um exemplo:

```Rust
fn capitalizar(s: &str) -> String {
    let mut resultado = String::new();
    let mut uppercase_next = true;
    for c in s.chars() {
        let ch = if uppercase_next {
            c.to_uppercase().collect::<String>()
        } else {
            c.to_lowercase().collect::<String>()
        };
        resultado.push_str(&ch);
        uppercase_next = c.is_whitespace();
    }
    resultado
}

fn main() {
    let minha_string = "olá, mundo!";

    println!("{}", capitalizar(minha_string)); 
    // saída: "Olá, Mundo!"
}
```

## Mergulho Profundo

Historicamente, capitalização surgiu para facilitar a leitura, principalmente em idiomas latinos, sendo a técnica utilizada também em programação por esse motivo.

Em Rust, além do método manual apresentado, há alternativas como o uso de bibliotecas externas que oferecem funções para esse fim, como a `titlecase`. No entanto, a abordagem acima é a mais conveniente por não demandar dependências extras.

A implementação acima funciona da seguinte maneira: para cada caractere da string, verifica se deve ser convertido para maiúsculo (nesse caso, quando o caractere anterior era um espaço) ou minúsculo. 

## Veja Também

1. Documentação oficial `str.chars`: https://doc.rust-lang.org/std/primitive.str.html#method.chars
2. Documentação oficial `str.is_whitespace`: https://doc.rust-lang.org/std/primitive.str.html#method.is_whitespace
3. Documentação oficial `char.to_uppercase`: https://doc.rust-lang.org/std/primitive.char.html#method.to_uppercase
4. Documentação oficial `char.to_lowercase`: https://doc.rust-lang.org/std/primitive.char.html#method.to_lowercase
5. Documentação oficial `str.push_str`: https://doc.rust-lang.org/std/string/struct.String.html#method.push_str
6. Biblioteca `titlecase`: https://crates.io/crates/titlecase