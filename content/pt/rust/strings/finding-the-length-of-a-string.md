---
title:                "Descobrindo o comprimento de uma string"
date:                  2024-01-20T17:48:25.197261-07:00
model:                 gpt-4-1106-preview
simple_title:         "Descobrindo o comprimento de uma string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
Descobrir o tamanho de uma string significa saber quantos caracteres ela possui. Programadores precisam dessa informação para, por exemplo, validar entradas, otimizar algoritmos ou manipular texto de forma eficiente.

## Como Fazer:

```Rust
fn main() {
    let minha_string = "Olá, mundo!";
    let tamanho = minha_string.chars().count(); // Conta os caracteres Unicode corretamente
    println!("O tamanho da string é: {}", tamanho);
    // Saída: O tamanho da string é: 12
}
```

Se sua string usar somente ASCII, você pode usar um método mais direto:

```Rust
fn main() {
    let minha_string = "Olá, mundo!";
    let tamanho = minha_string.len(); // Bom para ASCII, pode ser impreciso para Unicode
    println!("O tamanho da string (em bytes) é: {}", tamanho);
    // Saída: O tamanho da string (em bytes) é: 13
}
```

## Aprofundando

Em Rust, strings são uma sequência de bytes, e isso complica um pouco as coisas. Há algumas maneiras de contar 'tamanho', e tudo depende de como você encara uma string.

- **len**: Retorna o número de bytes. Ótimo para eficiência quando você sabe que está lidando com ASCII. Mas cuidado: com caracteres Unicode (como acentos ou emojis), você terá um número de bytes, não de caracteres.
- **chars().count()**: Conta os caracteres como elementos de 'char', que são pontos de código Unicode. Grátis para erros de multibyte, mas mais lento porque precisa interpretar a string.

Historicamente, a escolha de Rust para lidar com UTF-8 como formato padrão reflete uma preferência moderna por compatibilidade global, em vez da simplicidade do ASCII.

Alternativas: Bibliotecas especializadas podem oferecer maneiras diferentes de manipular e medir strings, algumas focadas em desempenho, outras em flexibilidade.

Implementação: Na prática, contar caracteres Unicode corretamente é complicado. Rust lida com isso ao expor diferentes métodos para diferentes situações, encorajando o programador a escolher conscientemente como medir suas strings.

## Veja Também

- Documentação Oficial de Rust sobre Strings: [https://doc.rust-lang.org/book/ch08-02-strings.html](https://doc.rust-lang.org/book/ch08-02-strings.html)
- Unicode Standard: [https://unicode.org/standard/standard.html](https://unicode.org/standard/standard.html)
