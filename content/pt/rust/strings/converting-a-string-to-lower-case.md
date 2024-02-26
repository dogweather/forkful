---
date: 2024-01-20 17:39:12.643401-07:00
description: "Converter uma string para min\xFAsculas significa transformar todos\
  \ os caracteres alfab\xE9ticos do texto para sua forma min\xFAscula. Programadores\
  \ fazem isso\u2026"
lastmod: '2024-02-25T18:49:43.976499-07:00'
model: gpt-4-1106-preview
summary: "Converter uma string para min\xFAsculas significa transformar todos os caracteres\
  \ alfab\xE9ticos do texto para sua forma min\xFAscula. Programadores fazem isso\u2026"
title: "Convertendo uma string para min\xFAsculas"
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
