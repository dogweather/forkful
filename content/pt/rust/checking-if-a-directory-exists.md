---
title:                "Verificando se um diretório existe"
date:                  2024-01-20T14:58:30.317696-07:00
html_title:           "Fish Shell: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Descobrir se um diretório existe é simplesmente verificar se uma determinada pasta está presente no sistema de arquivos. Os programadores fazem isso para evitar erros ao tentar acessar, ler ou escrever em um diretório que não está lá.

## Como Fazer:
Para checar a existência de um diretório em Rust, você pode usar o módulo `std::fs` e a função `metadata` ou então a função `path.exists`, como mostrado abaixo:

```Rust
use std::fs;
use std::path::Path;

fn main() {
    let dir = Path::new("/algum/diretorio");

    // Usando metadata 
    match fs::metadata(dir) {
        Ok(metadata) => {
            if metadata.is_dir() {
                println!("O diretório existe!");
            } else {
                println!("Existe, mas não é um diretório!");
            }
        }
        Err(e) => println!("O diretório não existe: {}", e),
    }

    // Usando exists
    if dir.exists() {
        println!("O diretório existe!");
    } else {
        println!("O diretório não existe.");
    }
}
```

## Mergulho Profundo
Historicamente, verificar a presença de um diretório é uma operação comum em muitas linguagens de programação, porque a interação com o sistema de arquivos é uma necessidade básica para muitos programas. No Rust, a checagem é feita utilizando a biblioteca padrão com funções como `fs::metadata` que pode fornecer mais do que a simples existência de um caminho, contendo informações detalhadas como permissões e timestamps. Alternativamente, para uma checagem rápida, `Path::exists` é uma opção direta e menos verbosa. A escolha entre eles pode depender se você precisa ou não de informações adicionais ou apenas validar a existência.

Desde Rust 1.5, existe um tratamento de caminhos UTF-8 como uma camada intermediária entre os Strings e os caminhos do sistema de arquivos, que os torna mais portáteis e seguros.

## Veja Também
Para mais detalhes sobre as funções que você pode usar para interagir com o sistema de arquivos em Rust, veja a documentação oficial:

- [`std::path::Path`](https://doc.rust-lang.org/std/path/struct.Path.html)
- [`std::fs`](https://doc.rust-lang.org/std/fs/index.html)
- [Livro de Rust - Capítulo sobre File I/O](https://doc.rust-lang.org/book/ch12-00-an-io-project.html)
