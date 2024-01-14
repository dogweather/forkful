---
title:                "Rust: Escrevendo um arquivo de texto"
simple_title:         "Escrevendo um arquivo de texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Rust?

Escrever um arquivo de texto pode ser uma tarefa simples, mas ao realizar em Rust, aproveitamos todas as vantagens de uma linguagem de programação moderna e de alto desempenho. Além disso, ao escrever um arquivo de texto em Rust, podemos utilizar os recursos da linguagem para tornar nosso código mais eficiente e seguro.

## Como fazer:

Para escrever um arquivo de texto em Rust, utilizamos o pacote `std::fs`, que nos permite acessar e manipular arquivos. Primeiro, precisamos importar o pacote em nosso código:

```Rust
use std::fs::File;
```

Em seguida, podemos criar um novo arquivo de texto utilizando a função `File::create()` e passando o nome do arquivo como parâmetro. Por exemplo, se quisermos criar um arquivo chamado "meu_arquivo.txt", o código ficaria assim:

```Rust
let arquivo = File::create("meu_arquivo.txt").expect("Erro ao criar arquivo!");
```

Após criar o arquivo, podemos escrever conteúdo nele utilizando o método `write_all()` e passando uma string com o conteúdo desejado como parâmetro. Por exemplo:

```Rust
arquivo.write_all(b"Olá, mundo!").expect("Erro ao escrever no arquivo!");
```

Também podemos utilizar a função `File::open()` para abrir um arquivo já existente e escrever nele. E para garantir que o arquivo seja fechado corretamente ao final do nosso código, podemos utilizar o método `drop()`:

```Rust
let arquivo = File::open("arquivo.txt").expect("Erro ao abrir arquivo!");
// Escreve "Texto de exemplo" no arquivo
arquivo.write_all(b"Texto de exemplo").expect("Erro ao escrever no arquivo!");
drop(arquivo);
```

## Mergulho profundo:

Ao escrever um arquivo de texto em Rust, também podemos utilizar a estrutura `BufWriter` para melhorar o desempenho do nosso código. Essa estrutura armazena os dados em um buffer antes de escrevê-los no arquivo, evitando muitas chamadas ao sistema operacional. Por exemplo:

```Rust
use std::fs::File;
use std::io::Write;
use std::io::BufWriter;

let arquivo = File::create("arquivo.txt").expect("Erro ao criar arquivo!");
// Cria um BufWriter, passando uma referência para o arquivo e o tamanho do buffer
let mut escritor = BufWriter::new(arquivo, 1024);
// Escreve "Texto de exemplo" no arquivo
escritor.write_all(b"Texto de exemplo").expect("Erro ao escrever no arquivo!");
// Descarrega todos os dados pendentes no buffer antes de fechar o arquivo
escritor.flush().expect("Erro ao descarregar dados do buffer!");
```

Com isso, podemos observar uma melhoria significativa no desempenho ao escrever no arquivo.

## Veja também:

- [Documentação oficial: std::fs](https://doc.rust-lang.org/std/fs/index.html)
- [Tutorial de Rust: Trabalhando com arquivos](https://www.rustbrasil.com/comecando-com-rust/trabalhando-com-arquivos/)
- [Artigo: Manipulando arquivos em Rust](https://medium.com/@joseanimacoes/programa%C3%A7%C3%A3o-em-rust-manipulando-arquivos-1d5143a501e9)