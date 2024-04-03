---
date: 2024-01-20 17:58:46.368494-07:00
description: "Substituir texto numa string \xE9 como trocar pe\xE7as de roupa; voc\xEA\
  \ pega uma palavra ou frase e troca por outra. Programadores fazem isso para atualizar\u2026"
lastmod: '2024-03-13T22:44:46.353419-06:00'
model: gpt-4-1106-preview
summary: "Substituir texto numa string \xE9 como trocar pe\xE7as de roupa; voc\xEA\
  \ pega uma palavra ou frase e troca por outra."
title: Pesquisando e substituindo texto
weight: 10
---

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
