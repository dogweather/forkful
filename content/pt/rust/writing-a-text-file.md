---
title:    "Rust: Escrevendo um arquivo de texto"
keywords: ["Rust"]
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto com Rust?

Escrever um arquivo de texto pode ser útil em uma variedade de situações, como salvar dados gerados em um programa ou criar um arquivo de configuração para uma aplicação. Com Rust, é possível utilizar o tipo de dado `File` para criar e manipular arquivos de texto de forma eficiente e segura.

## Como fazer

Para começar, é necessário importar o módulo `std::fs`, que contém as funções para trabalhar com arquivos. Em seguida, utilizamos a função `create` para criar um novo arquivo e atribuí-lo a uma variável:

```Rust
use std::fs;

let arquivo = fs::File::create("arquivo.txt").expect("Não foi possível criar o arquivo!");
```

Agora, podemos utilizar a função `write` para escrever conteúdo no arquivo recém-criado. Essa função espera como parâmetro um slice de bytes, que pode ser criado a partir de uma string utilizando o método `as_bytes`:

```Rust
use std::fs;
use std::io::Write;

let arquivo = fs::File::create("arquivo.txt").expect("Não foi possível criar o arquivo!");
arquivo.write("Olá, mundo!".as_bytes()).expect("Não foi possível escrever no arquivo!");
```

Além de escrever, também é possível ler dados de um arquivo utilizando a função `read_to_string`:

```Rust
use std::fs;

let mut arquivo = fs::File::open("arquivo.txt").expect("Não foi possível abrir o arquivo!");
let mut conteudo = String::new();
arquivo.read_to_string(&mut conteudo).expect("Não foi possível ler o arquivo!");

println!("{}", conteudo); // imprime "Olá, mundo!"
```

## Aprofundando mais

Escrever um arquivo de texto envolve utilizar diferentes conceitos da linguagem Rust, como a gestão de erros (`Result`), tipos de dados (`File`), módulos e funções. Em casos mais complexos, também pode ser necessário utilizar o tipo `Path` para trabalhar com caminhos de arquivos. É importante entender esses conceitos para escrever um código robusto e eficiente.

## Veja também

- [Documentação oficial do módulo `std::fs`](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial: Escrita e leitura de arquivos em Rust](https://danielkeep.github.io/tlborm/book/preface.html)
- [Exemplo de manipulação de arquivos em Rust](https://github.com/lynn/HelloRust/blob/master/files/src/main.rs)