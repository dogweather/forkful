---
title:                "Excluindo caracteres que correspondem a um padrão"
aliases:
- /pt/rust/deleting-characters-matching-a-pattern/
date:                  2024-01-20T17:42:59.913326-07:00
model:                 gpt-4-1106-preview
simple_title:         "Excluindo caracteres que correspondem a um padrão"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Na programação, deletar caracteres que correspondem a um padrão significa identificar e remover séries específicas de caracteres dentro de uma string. Programadores fazem isso para limpar entradas de dados, processar texto e preparar informações para armazenamento ou análise posterior.

## Como Fazer:

Para remover caracteres que correspondem a um padrão em Rust, você pode usar o método `replace` ou expressões regulares com o crate `regex`. Aqui estão os exemplos:

```Rust
fn main() {
    let frase = "Filtro 123, fácil!";
    let somente_letras = frase.replace(|c: char| c.is_numeric(), "");
    println!("{}", somente_letras);
    // Saída: Filtro , fácil!
}
```

Com expressões regulares:

```Rust
use regex::Regex;

fn main() {
    let frase = "Filtro 123, fácil!";
    let regex = Regex::new(r"\d").unwrap();
    let resultado = regex.replace_all(&frase, "");
    println!("{}", resultado);
    // Saída: Filtro , fácil!
}
```

## Aprofundamento

Historicamente, manipular strings é uma tarefa comum em programação. Linguagens mais antigas já forneciam ferramentas para isso. Em Rust, além das funções básicas de string, a biblioteca `regex` oferece uma potente ferramenta para trabalhar com padrões complexos de caracteres.

Alternativas ao `replace` e `regex` incluem iterar manualmente sobre a string e construir uma nova string sem os caracteres indesejados, mas isso pode ser mais verboso e propenso a erros.

A implementação do `replace` é direta e eficiente para padrões simples, enquanto o `regex` é mais flexível e adequado para padrões complexos, mas pode ter um custo de desempenho maior.

## Veja Também

- Documentação oficial do Rust sobre manipulação de strings: [std::string](https://doc.rust-lang.org/std/string/)
- Livro 'The Rust Programming Language': [Manipulação de Strings e Texto](https://doc.rust-lang.org/book/ch08-02-strings.html)
