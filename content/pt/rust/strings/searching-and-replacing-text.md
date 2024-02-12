---
title:                "Pesquisando e substituindo texto"
aliases: - /pt/rust/searching-and-replacing-text.md
date:                  2024-01-20T17:58:46.368494-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pesquisando e substituindo texto"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Substituir texto numa string é como trocar peças de roupa; você pega uma palavra ou frase e troca por outra. Programadores fazem isso para atualizar dados, corrigir erros, ou formatar informação de maneira mais digerível.

## Como fazer:

Usaremos a biblioteca `regex` para procurar e substituir texto, garantindo mais potência e flexibilidade do que métodos padrão:

```Rust
use regex::Regex;

fn main() {
    let texto = "Ferrugem é o futuro da programação de sistemas.";
    let regex = Regex::new("Ferrugem").unwrap();
    let novo_texto = regex.replace(texto, "Rust");
    println!("{}", novo_texto);
}
```

Saída:

```
Rust é o futuro da programação de sistemas.
```

## Aprofundando:

A substituição de texto é algo prático desde os primórdios da computação, onde editar arquivos batch e scripts era essencial. Hoje, em Rust, além da `regex`, temos métodos nativos como `replace()` para trocas simples.

Alternativas incluem bibliotecas como `strsim` para comparação de strings e `aho_corasick` para múltiplas substituições eficientes. A implementação usando `regex` é poderosa por usar expressões regulares, permitindo substituições complexas e padrões dinâmicos.

## Veja também:

- Documentação oficial do Rust sobre manipulação de strings: https://doc.rust-lang.org/book/ch08-02-strings.html
- Crate `regex`: https://crates.io/crates/regex
- Crate `strsim`: https://crates.io/crates/strsim
- Crate `aho_corasick`: https://crates.io/crates/aho_corasick
