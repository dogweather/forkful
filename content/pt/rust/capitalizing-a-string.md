---
title:                "Capitalizando uma string"
html_title:           "Bash: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Transformar uma string em maiúsculas significa alterar todos os caracteres alfabéticos para a sua forma maiúscula. Programadores fazem isso para uniformizar dados, garantir consistência para comparações, ou atender requisitos estéticos e de apresentação.

## Como Fazer:
```Rust
fn main() {
    let minha_string = "olá, mundo!";
    let string_maiuscula = minha_string.to_uppercase();

    println!("Original: {}", minha_string);
    println!("Maiúscula: {}", string_maiuscula);
}
```
O programa acima imprimiria:
```
Original: olá, mundo!
Maiúscula: OLÁ, MUNDO!
```

## Mergulho Profundo
O método `to_uppercase()` em Rust utiliza a biblioteca `unicode-segmentation` para iterar sobre cada ponto de código Unicode da string e convertê-los para sua forma maiúscula. Historicamente, a capitalização era mais simples em ASCII, mas o suporte global a Unicode introduziu complexidades, como caracteres que não têm uma forma maiúscula direta ou que mudam de tamanho ao serem capitalizados.

Existem alternativas, como o método `make_ascii_uppercase()` para strings que você sabe serem ASCII e o crate `unicase` para comparações que ignoram a caixa.

A implementação do Rust garante que todas as localidades sejam respeitadas, importante em contextos internacionais.

## Veja Também
- Rust docs on Strings: https://doc.rust-lang.org/stable/std/string/struct.String.html
- Unicode Segmentation: https://unicode.org/reports/tr29/
- Unicase crate: https://crates.io/crates/unicase
