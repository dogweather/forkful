---
title:                "Verificando se um diretório existe"
html_title:           "Rust: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Rust"
category:             "Rust"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Você provavelmente já se deparou com a necessidade de verificar se um diretório existe durante a programação. Isso pode ocorrer ao manipular arquivos ou ao criar uma estrutura de diretórios para armazenar dados. Neste artigo, vamos explorar como realizar essa tarefa usando a linguagem de programação Rust.

## Como fazer

Para verificar se um diretório existe em Rust, podemos usar a função `exists()` do módulo `std::fs`. Essa função retorna um booleano indicando se o diretório especificado existe ou não.

```
use std::fs;

fn main() {
    if fs::exists("meu_diretorio") {
        println!("O diretório existe!");
    } else {
        println!("O diretório não existe.");
    }
}
```

No código acima, usamos a função `exists()` para verificar se o diretório "meu_diretorio" existe. Se existir, imprimimos uma mensagem indicando isso. Caso contrário, imprimimos uma mensagem informando que o diretório não existe.

Além disso, também podemos usar a função `create_dir()` para criar um diretório caso ele não exista. Esta função recebe como parâmetro o nome do diretório que desejamos criar.

```
use std::fs;

fn main() {
    if fs::exists("meu_diretorio") {
        println!("O diretório existe!");
    } else {
        fs::create_dir("meu_diretorio").expect("Falha ao criar diretório.");
    }
}
```

No exemplo acima, primeiro verificamos se o diretório "meu_diretorio" existe. Caso não exista, criamos o diretório usando a função `create_dir()`. O uso de `expect()` é opcional, mas é uma boa prática para lidar com possíveis erros ao criar o diretório.

## Mergulho profundo

É importante notar que a função `exists()` não diferencia diretórios de arquivos. Ela simplesmente verifica se um arquivo ou diretório com o nome especificado existe no caminho especificado. Portanto, esta função também pode ser usada para verificar a existência de arquivos.

Além disso, se você precisar verificar se um diretório existe em um determinado local, pode usar o método `exists()` da estrutura `Path` do módulo `std::path`.

```
use std::path::Path;

fn main() {
    let caminho = Path::new("./meu_diretorio/");
    
    if caminho.exists() {
        println!("O diretório existe!");
    } else {
        println!("O diretório não existe.");
    }
}
```

No código acima, criamos um objeto `Path` para representar o caminho para o nosso diretório "meu_diretorio". Em seguida, usamos o método `exists()` da estrutura `Path` para verificar se o diretório existe.

## Veja também

- [Documentação da função `exists()` do módulo `std::fs`](https://doc.rust-lang.org/std/fs/fn.exists.html)
- [Documentação do método `exists()` da estrutura `Path` do módulo `std::path`](https://doc.rust-lang.org/std/path/struct.Path.html#method.exists)