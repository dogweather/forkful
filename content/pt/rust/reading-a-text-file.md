---
title:    "Rust: Lendo um arquivo de texto"
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Rust?

Se você é um programador que está começando a aprender Rust ou já tem experiência com a linguagem, é importante entender como ler e manipular arquivos de texto. Isso pode ser especialmente útil para a entrada e saída de dados em aplicativos e scripts. Neste artigo, vamos mostrar como ler um arquivo de texto em Rust e explorar algumas técnicas avançadas para lidar com esse tipo de arquivo.

## Como fazer:

Para ler um arquivo de texto, primeiro precisamos importar a biblioteca padrão `std::fs::File` e a biblioteca `std::io::Read` que contém métodos para ler do arquivo. Então, podemos usar o método `read_to_string()` para ler todo o conteúdo do arquivo em uma string. Veja um exemplo abaixo:

```rust
use std::fs::File;
use std::io::Read;

fn main() {
    let mut arquivo = File::open("exemplo.txt").expect("Não foi possível abrir o arquivo!"); // abre o arquivo
    let mut conteudo = String::new(); // cria uma string vazia para armazenar o conteúdo do arquivo
    arquivo.read_to_string(&mut conteudo).expect("Não foi possível ler o arquivo!"); // lê o conteúdo do arquivo e armazena na string
    println!("{}", conteudo); // imprime o conteúdo do arquivo
}
```

A saída para esse código seria o conteúdo do arquivo `exemplo.txt`. É importante lembrar que o método `read_to_string()` lê o arquivo inteiro de uma vez, então é preciso ter cuidado com arquivos muito grandes que possam causar problemas de desempenho.

## Mergulho profundo:

Além do método `read_to_string()`, existem outras formas de ler arquivos de texto em Rust. Uma delas é o método `read_line()`, que lê o arquivo linha por linha em um laço `while`. Veja um exemplo:

```rust
use std::fs::File;
use std::io::BufReader;
use std::io::prelude::*;

fn main() {
    let arquivo = File::open("exemplo.txt").expect("Não foi possível abrir o arquivo!"); // abre o arquivo
    let leitor = BufReader::new(arquivo); // cria um leitor que lê o arquivo linha por linha
    for linha in leitor.lines() { // itera sobre cada linha no arquivo
        let linha = linha.expect("Não foi possível ler a linha!"); // transforma a linha em uma string
        println!("{}", linha); // imprime a linha
    }
}
```

Além disso, também é possível especificar o número máximo de bytes a serem lidos pelo método `read_to_string()` ou utilizar a biblioteca `std::io::BufRead` para ler o arquivo de forma mais eficiente.

## Veja também:

Aqui estão alguns links úteis para aprender mais sobre como ler arquivos de texto em Rust:

- [Documentação oficial sobre leitura de arquivos em Rust](https://doc.rust-lang.org/std/fs/struct.File.html#method.read_to_string)

- [Tutorial sobre leitura de arquivos em Rust](https://www.youtube.com/watch?v=zF34dRivLOw)

- [Exemplos de código em Rust para leitura de arquivos](https://github.com/bbkane/rust-examples/tree/master/src/file-io)