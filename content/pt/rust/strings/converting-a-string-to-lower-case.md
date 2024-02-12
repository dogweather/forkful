---
title:                "Convertendo uma string para minúsculas"
aliases:
- /pt/rust/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:39:12.643401-07:00
model:                 gpt-4-1106-preview
simple_title:         "Convertendo uma string para minúsculas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O Que & Por quê?
Converter uma string para minúsculas significa transformar todos os caracteres alfabéticos do texto para sua forma minúscula. Programadores fazem isso para padronizar entradas de dados, facilitando comparações e buscas insensíveis a maiúsculas/minúsculas.

## Como fazer:
```Rust
fn main() {
    let texto = "Rust é LEGAL!";
    let texto_minusculo = texto.to_lowercase();
    println!("{}", texto_minusculo);
}
```
Saída:
```
rust é legal!
```

## Mergulho Profundo
Historicamente, a conversão de strings para caixa baixa é essencial em várias operações de software, como comparação de dados e ordenação alfabética. No Rust, o método `.to_lowercase()` é definido pelo trait `Unicode` e difere de `.to_ascii_lowercase()`, que só afeta caracteres ASCII. O Rust considera pontos de código Unicode, fornecendo uma operação de conversão mais completa e globalmente relevante que é diferente da oferecida em linguagens como C e Java, onde as operações podem se basear apenas na tabela ASCII.

Alternativas ao `.to_lowercase()` incluem o uso de bibliotecas de terceiros que podem oferecer controle mais granular sobre a conversão de caracteres ou manipulação direta de bytes se estiver trabalhando apenas com ASCII.

## Veja Também
- [Unicode case mapping](https://www.unicode.org/reports/tr21/tr21-5.html)
- [Documentação Rust para `String`](https://doc.rust-lang.org/std/string/struct.String.html)
- [Rust by Example - Strings](https://doc.rust-lang.org/stable/rust-by-example/std/str.html)
