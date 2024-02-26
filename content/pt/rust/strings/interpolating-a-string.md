---
date: 2024-01-20 17:51:35.087329-07:00
description: "Interpola\xE7\xE3o de string \xE9 o processo de inserir valores de vari\xE1\
  veis dentro de uma string. Programadores fazem isso para construir strings din\xE2\
  micas que\u2026"
lastmod: '2024-02-25T18:49:43.975611-07:00'
model: gpt-4-1106-preview
summary: "Interpola\xE7\xE3o de string \xE9 o processo de inserir valores de vari\xE1\
  veis dentro de uma string. Programadores fazem isso para construir strings din\xE2\
  micas que\u2026"
title: Interpolando uma string
---

{{< edit_this_page >}}

## O Que & Por Quê?
Interpolação de string é o processo de inserir valores de variáveis dentro de uma string. Programadores fazem isso para construir strings dinâmicas que incluem dados variáveis, como nomes de usuários, pontuações em jogos ou resultados de cálculos.

## Como Fazer:
Em Rust, interpolação de string é comumente feita com a macro `format!` ou com a macro `println!` para imprimir diretamente no console. Veja os exemplos:

```Rust
fn main() {
    let nome = "Fernanda";
    let idade = 28;
    
    // Usando a macro format!
    let mensagem = format!("Olá, {}! Você tem {} anos.", nome, idade);
    println!("{}", mensagem);
    
    // Usando a macro println! para imprimir diretamente
    println!("Oi, {}! No próximo ano, você terá {} anos.", nome, idade + 1);
}
```

Saída:
```
Olá, Fernanda! Você tem 28 anos.
Oi, Fernanda! No próximo ano, você terá 29 anos.
```

## Aprofundando
A interpolação de string não é uma característica única do Rust; muitas outras linguagens de programação oferecem funcionalidades semelhantes. No entanto, a segurança de tipos do Rust garante que os valores inseridos na string sejam do tipo esperado, evitando erros comuns em outras linguagens.

Alternativas como a concatenação de strings com o operador `+` são menos elegantes e eficientes em Rust devido à propriedade de ownership que o torna mais complexo para strings do que tipos primitivos.

O mecanismo de funcionamento interno das macros `format!` e `println!` usa traits como `Display` e `Debug` — `Display` para uma formatação bonita e destinada ao usuário final e `Debug` para uma saída mais crua e útil para debug.

## Veja Também
- Documentação oficial do Rust sobre a macro `format!`: [https://doc.rust-lang.org/std/fmt/#syntax](https://doc.rust-lang.org/std/fmt/#syntax)
- Rust by Example sobre interpolação de string e formatação: [https://doc.rust-lang.org/rust-by-example/hello/print.html](https://doc.rust-lang.org/rust-by-example/hello/print.html)
- Para entender melhor a propriedade de ownership em Rust: [https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html](https://doc.rust-lang.org/book/ch04-01-what-is-ownership.html)
