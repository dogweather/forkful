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

## O que & Por que?
Capitalizar uma string é transformar todas as letras em maiúsculas. Os programadores geralmente fazem isso para padronizar strings e torná-las mais legíveis.

## Como fazer:
```Rust
let string = "exemplo";
let capitalized_string = string.to_uppercase();
println!("{}", capitalized_string);
```

Saída: "EXEMPLO"

## Profundidade:
Embora capitalizar strings possa parecer uma tarefa simples, há algumas coisas a serem consideradas ao implementá-la. Antes do desenvolvimento de linguagens de programação modernas, capitalizar strings era geralmente feito por meio de conversão manual de caracteres. Felizmente, com o Rust, podemos usar o método `to_uppercase()` para capitalizar facilmente nossas strings.

## Veja também:
- [Documentação oficial do Rust sobre `to_uppercase()`](https://doc.rust-lang.org/std/primitive.str.html#method.to_uppercase)
- [Guia prático de formatação de strings em Rust](https://blog.logrocket.com/practical-guide-formatting-strings-rust/)