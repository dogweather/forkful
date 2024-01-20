---
title:                "Lendo um arquivo de texto"
html_title:           "Bash: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lendo Arquivos de Texto No Rust

## O Qual e Por que?
Ler um arquivo de texto significa acessar e interpretar as informações nele contidas. Os programadores fazem isso para obter dados necessários para um software, seja para configuração, análise de dados ou entrada do usuário.

## Como fazer:

No Rust, podemos usar o módulo `std::fs::read_to_string` para ler um arquivo de texto. Basta passar o nome do arquivo para o método. 

```Rust
use std::fs;

fn main() {
    let conteúdo = fs::read_to_string("exemplo.txt").unwrap();
    println!("Conteúdo do arquivo: {}", conteúdo);
}
```

Se tudo correr bem, o código acima imprimirá o conteúdo do arquivo `exemplo.txt`.

## Mergulho Profundo

Rust tem uma abordagem diferente quando se trata de ler arquivos de texto. Fortemente inspirado pela filosofia Unix de pequenas ferramentas fazendo bem seu trabalho e a integração perfeita com outras.

Alternativas como `BufRead::lines` existem se você deseja ler linha por linha em vez de ler todo o arquivo como uma única `String`.

O detalhe de implementação que você talvez queira estar ciente no Rust é o uso de `unwrap`. Este é um atalho conveniente para lida com um tipo `Result`. No entanto, é uma boa prática manejar erros de uma maneira mais estruturada em um programa real.

## Ver também

1. [Documentação oficial do Rust para fs](https://doc.rust-lang.org/std/fs/index.html)
2. [Lendo arquivos no Rust - Stack Overflow](https://stackoverflow.com/questions/31192956/whats-the-de-facto-way-of-reading-and-writing-files-in-rust-1-x)
3. [Uma introdução à programação Rust - Livro oficial](https://doc.rust-lang.org/book/)