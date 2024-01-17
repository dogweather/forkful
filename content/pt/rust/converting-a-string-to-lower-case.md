---
title:                "Convertendo uma string para minúsculas"
html_title:           "Rust: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O que e Por que?
Converter uma string para letras minúsculas é um processo no qual todas as letras maiúsculas de uma string são transformadas em letras minúsculas. Isso é feito principalmente para tornar as strings mais uniformes e facilitar comparações ou operações de busca. 

## Como Fazer:
Para converter uma string para letras minúsculas em Rust, podemos usar o método to_lowercase() da biblioteca padrão Strings. Aqui está um exemplo de código:

```Rust
let minha_string = "Exemplo de STRING";
let string_minúscula = minha_string.to_lowercase();
println!("Minha string em letras minúsculas é: {}", string_minúscula);
```

A saída deste código seria: 
```
Minha string em letras minúsculas é: exemplo de string
```

## Deep Dive:
O processo de converter strings em letras minúsculas é comum em muitas linguagens de programação e tem sido usado desde os primeiros dias da computação. Em Rust, a função to_lowercase() existe desde a versão 1.0 da linguagem e é usada principalmente para padronizar strings em operações de comparação, como no exemplo abaixo:

```Rust
let minha_string = "Hello";
let outra_string = "hello";
if minha_string == outra_string {
    println!("As strings são iguais!");
} else {
    println!("As strings são diferentes!");
}
```

A saída deste código seria:
```
As strings são diferentes!
```

Alternativamente, também podemos usar o método to_ascii_lowercase() para converter uma string apenas para caracteres ASCII minúsculos. Isso pode ser útil quando queremos ignorar acentos e caracteres especiais em operações de comparação.

É importante notar que o método to_lowercase() é sensível à localização (locale-sensitive) e pode produzir resultados diferentes dependendo do idioma ou configurações do sistema. Isso pode ser evitado usando o método to_lowercase_raw() ou to_ascii_lowercase(), que são insensíveis à localização.

## Veja Também:
- Documentação do método to_lowercase(): [https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase](https://doc.rust-lang.org/std/string/struct.String.html#method.to_lowercase)
- Documentação do método to_ascii_lowercase(): [https://doc.rust-lang.org/std/string/struct.String.html#method.to_ascii_lowercase](https://doc.rust-lang.org/std/string/struct.String.html#method.to_ascii_lowercase)
- Outras funções de manipulação de strings em Rust: [https://doc.rust-lang.org/std/string/fn.to_lowercase.html#example](https://doc.rust-lang.org/std/string/fn.to_lowercase.html#example)