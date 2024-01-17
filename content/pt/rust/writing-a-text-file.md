---
title:                "Escrevendo um arquivo de texto"
html_title:           "Rust: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## O que e Porquê?

Escrever um arquivo de texto é simplesmente criar um arquivo que contém informações em formato de texto, como palavras, números ou símbolos. Os programadores geralmente escrevem arquivos de texto para armazenar dados ou informações importantes que são usadas em seus programas.

## Como Fazer:

Em Rust, escrever um arquivo de texto é bem fácil e pode ser feito usando a função ```std::fs::write()```. Primeiro, importe o módulo de sistema de arquivos usando a macro ```use std::fs;```. Em seguida, crie seu arquivo de texto usando a função ```write()``` e passe o nome do arquivo, seguido pelos dados que deseja gravar no arquivo. Veja o exemplo abaixo:

```
use std::fs;

fn main() {
    let dados = r#"Meu primeiro arquivo de texto!"#;
    
    fs::write("arquivo.txt", dados).expect("Falha ao escrever no arquivo!");
    
    println!("Arquivo criado com sucesso!");
}
```

Se tudo ocorrer corretamente, você verá a mensagem "Arquivo criado com sucesso!" impressa no terminal. E se você abrir o arquivo "arquivo.txt", verá que seu texto foi gravado no arquivo.

## Mergulho Profundo

Escrever arquivos de texto é uma técnica comum usada em muitas linguagens de programação, incluindo Rust. Antes da introdução do sistema de arquivos padrão em Rust, os programadores usavam bibliotecas externas para escrever arquivos de texto. No entanto, o sistema de arquivos padrão se tornou a maneira mais simples e confiável de fazer isso.

Existem também outras formas de escrever arquivos de texto em Rust, como usar a macro ```print!()``` para escrever diretamente no arquivo, ou até mesmo criar uma estrutura para facilitar a escrita em arquivos. No entanto, o método usando a função ```write()``` é o mais comum e recomendado.

## Veja Também

Para saber mais sobre escrever arquivos de texto em Rust, confira a documentação oficial do Rust [documento] (https://doc.rust-lang.org/std/fs/fn.write.html). Você também pode explorar outras maneiras de escrever arquivos de texto em Rust e descobrir qual funciona melhor para você.