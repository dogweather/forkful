---
title:                "Rust: Procurando e substituindo texto"
programming_language: "Rust"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que?

A substituição de texto é uma tarefa comum na programação. É uma habilidade fundamental que permite aos programadores encontrar e substituir partes específicas de um texto em seus programas. Em Rust, existem várias opções disponíveis para realizar essa tarefa, então vamos explorar algumas das maneiras pelas quais você pode fazer isso.

## Como Fazer

#### Usando o método `replace`

A maneira mais simples de substituir texto em Rust é usando o método `replace`. Este método pertence à string que queremos alterar e aceita dois argumentos: o texto a ser substituído e o texto que queremos que seja substituído.

```rust
let texto = "Olá, como vai?".replace("O", "X");
println!("{}", texto);

// Saída: Xlá, como vai?
```

Neste exemplo, substituímos a letra "O" por "X" na string "Olá, como vai?". O método `replace` retornará uma nova string com a substituição aplicada.

#### Usando expressões regulares

Outra opção para substituir texto em Rust é usando expressões regulares. Com a ajuda da biblioteca `regex`, podemos procurar e substituir padrões específicos em uma string.

```rust
// Importando a biblioteca regex
use regex::Regex;

let texto = "Hoje é segunda, amanhã é terça".to_string();

// Definindo padrão e substituição
let padrao = Regex::new(r"segunda|terça").unwrap();
let substituicao = "quarta";

let resultado = padrao.replace_all(&texto, substituicao);
println!("{}", resultado);

// Saída: Hoje é quarta, amanhã é quarta
```

Neste exemplo, usamos a biblioteca `regex` para procurar por qualquer ocorrência de "segunda" ou "terça" na string e substituí-las por "quarta". É importante notar que o método `replace_all` retorna uma nova string, mantendo a original intacta.

## Deep Dive

Existem muitas maneiras de substituir texto em Rust, incluindo o uso de bibliotecas externas, como `regex`. Outras opções a serem exploradas incluem os métodos `replace_range` e `replace_range_unchecked`, que permitem substituir uma parte específica de uma string por outra. Além disso, também é possível usar o tipo `Cow` para lidar com diferentes tipos de dados.

## Veja Também

- Documentação oficial do Rust: https://www.rust-lang.org/learn
- Exemplos de substituição de texto em Rust: https://rust-lang-nursery.github.io/rust-cookbook/text/replace.html
- Tutorial de expressões regulares em Rust: https://docs.rs/regex/1.3.0/regex/#regular-expressions