---
title:                "Rust: Lendo um arquivo de texto"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Por que ler um arquivo de texto em Rust?

Ler e manipular arquivos de texto é uma tarefa comum em muitos projetos de programação, e o Rust oferece uma maneira eficiente e segura de lidar com essa tarefa. Neste artigo, vamos explorar como ler um arquivo de texto em Rust.

## Como fazer

Antes de começar a trabalhar com um arquivo de texto em Rust, primeiro precisamos abrir o arquivo. Isso pode ser feito usando a função `File::open()`, que recebe o caminho do arquivo como parâmetro e retorna um objeto `Result<T, E>`. Este objeto pode conter um valor `Ok(T)` se o arquivo for aberto com sucesso, ou um valor `Err(E)` se ocorrer algum erro durante a abertura do arquivo.

```Rust
use std::fs::File;

let file = File::open("arquivo.txt");
```

Uma vez que o arquivo é aberto com sucesso, podemos ler seu conteúdo usando o método `read_to_string()`, que converte automaticamente o conteúdo do arquivo em uma string.

```Rust
use std::fs::File;
use std::io::Read;

let mut file = File::open("arquivo.txt").expect("Não foi possível abrir o arquivo");

let mut conteudo = String::new();
file.read_to_string(&mut conteudo).expect("Não foi possível ler o arquivo");

println!("{}", conteudo);
```

O código acima abre o arquivo "arquivo.txt" e lê o seu conteúdo para uma string, que é impressa na tela.

## Mergulho Profundo

Ao trabalhar com arquivos de texto em Rust, também é importante lembrar de fechar o arquivo após a leitura. Isso pode ser feito usando o método `close()` no objeto `File`.

Além disso, é possível especificar o modo de leitura do arquivo ao abrir o arquivo. Por padrão, o arquivo é aberto em modo de leitura apenas, mas é possível especificar o modo de leitura e escrita usando os modos `r+` ou `w+` no método `File::open()`.

Outro ponto importante a ser considerado ao ler arquivos de texto é o tratamento de erros. Como mencionado anteriormente, a função `File::open()` retorna um objeto `Result`, que deve ser manipulado corretamente para tratar possíveis erros durante a abertura do arquivo.

## Veja também

- Documentação oficial do Rust para leitura de arquivos: https://doc.rust-lang.org/std/fs/struct.File.html
- Tutorial de leitura de arquivos em Rust: https://dev.to/ranadeep47/how-to-read-and-write-files-in-rust-18d1
- Exemplo prático de leitura de arquivos em Rust: https://github.com/jonathandturner/rust-snippets/blob/master/file_io.rs