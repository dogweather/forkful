---
title:                "Lendo um arquivo de texto."
html_title:           "Rust: Lendo um arquivo de texto."
simple_title:         "Lendo um arquivo de texto."
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## O que é e Porquê?
Ler um arquivo de texto é uma tarefa comum na programação, pois permite que os programadores acessem informações armazenadas em um arquivo de texto. Isso pode ser útil em várias situações, como salvar dados de um aplicativo, ler configurações ou analisar dados de um arquivo de log.

## Como fazer:
Para ler um arquivo de texto em Rust, primeiro precisamos abrir o arquivo usando a função "File::open()" e, em seguida, ler o conteúdo do arquivo usando o método "read_to_string()". Este método retornará uma string com todo o conteúdo do arquivo. Veja um exemplo abaixo:
```Rust
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let mut file = File::open("arquivo.txt").expect("Não foi possível abrir o arquivo");

    let mut conteudo = String::new();
    file.read_to_string(&mut conteudo).expect("Não foi possível ler o arquivo");

    println!("O conteúdo do arquivo é: {}", conteudo);
}
```
Exemplo de saída:
```
O conteúdo do arquivo é: Este é um arquivo de texto.
```

## Mais Detalhes:
Ler arquivos de texto em Rust é uma tarefa bem simples, graças à biblioteca padrão do Rust que fornece algumas funções úteis para trabalhar com arquivos. No entanto, há também outras bibliotecas de terceiros disponíveis, como a "fs_extra", que pode ser útil em cenários mais complexos, como copiar um arquivo para um diretório diferente.

## Veja Também:
- Documentação do Rust: https://doc.rust-lang.org/std/fs/struct.File.html
- Biblioteca fs_extra: https://crates.io/crates/fs_extra