---
title:                "Extraindo subcadeias"
html_title:           "Rust: Extraindo subcadeias"
simple_title:         "Extraindo subcadeias"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que e por quê?

Extrair substrings é uma técnica muito útil em programação que permite separar uma string em partes menores. Isso pode ser útil para manipular apenas uma parte específica de uma string ou para realizar operações em diferentes partes de uma string.

Os programadores geralmente fazem isso para tornar seu código mais legível e organizado, ou para realizar operações mais precisas em uma string.

## Como fazer:

Para extrair substrings em rust, você pode utilizar o método "slice" nativo da linguagem. Veja um exemplo de como isso pode ser feito:

```
let my_string = "Olá Rust!";
let substring = &my_string[4..8];
println!("{}", substring); // Saída: Rust
```

Neste exemplo, utilizamos o método "slice" para obter apenas a parte da string que vai do índice 4 ao 8. O resultado é a substring "Rust" que será impressa no console.

## Profundando:

Extrair substrings não é uma técnica nova e é amplamente utilizada em várias linguagens de programação. No entanto, em alguns casos, pode ser necessário utilizar outras abordagens, como usar expressões regulares, para obter substrings mais complexas.

Em Rust, é importante observar que o método "slice" retorna uma referência à substring original, o que torna o código mais eficiente, mas também pode apresentar alguns desafios em relação à propriedade de mutabilidade.

## Veja também:

- [Documentação oficial do Rust sobre Manipulação de strings](https://doc.rust-lang.org/stable/std/string/struct.String.html#method.slice)
- [Mais informações sobre expressões regulares em Rust](https://docs.rs/regex/1.3.9/regex/)
- [Guia de estilo informal para escrever em Rust](https://cheats.rs/#style)