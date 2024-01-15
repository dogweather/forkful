---
title:                "Lendo um arquivo de texto"
html_title:           "Rust: Lendo um arquivo de texto"
simple_title:         "Lendo um arquivo de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto?

Ler um arquivo de texto é uma tarefa básica e essencial para qualquer programador. Se você está iniciando sua jornada em programação ou trabalhando em um projeto complexo, ler arquivos de texto será inevitável. Portanto, é importante entender como realizar essa tarefa em Rust.

## Como fazer

Para ler um arquivo de texto em Rust, primeiro precisamos abrir o arquivo usando a função `File::open()`. Em seguida, podemos usar o método `read_to_string()` para armazenar o conteúdo do arquivo em uma variável como uma string. Por exemplo:

```Rust
use std::fs::File;
use std::io::Read;

fn main() {
    let mut file = File::open("meuarquivo.txt").expect("Não foi possível ler o arquivo");
    let mut conteudo = String::new();
    file.read_to_string(&mut conteudo).expect("Não foi possível ler o conteúdo do arquivo");
    println!("{}", conteudo);
}
```

Isso irá abrir o arquivo "meuarquivo.txt" e armazenar seu conteúdo na variável `conteudo`. Em seguida, usamos o `println!` para imprimir o conteúdo na tela.

## Mergulho profundo

Além de ler o conteúdo do arquivo inteiro, também podemos ler linha por linha ou até mesmo caractere por caractere usando o método `lines()` ou `chars()`, respectivamente. Além disso, podemos usar o tipo `BufReader` para melhorar o desempenho de leitura de arquivos maiores.

Outra funcionalidade útil é a capacidade de escrever em um arquivo usando o método `write_all()`. Para isso, é necessário abrir o arquivo com o modo de permissão `write` ou `append`.

## Veja também

Aqui estão alguns links úteis para aprofundar seu conhecimento em leitura e escrita de arquivos em Rust:

- [Documentação oficial de Rust sobre operações de arquivos](https://doc.rust-lang.org/std/fs/struct.File.html)
- [Tutorial sobre input e output em Rust](https://www.tutorialspoint.com/rust/rust_input_output.htm)
- [Exemplos de leitura e escrita de arquivos em Rust](https://buildamodule.com/forum/post/how-to-read-and-write-files-in-rust)