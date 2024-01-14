---
title:                "Rust: Verificando se um diretório existe"
programming_language: "Rust"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe

Muitas vezes, em projetos de programação, é necessário verificar se um determinado diretório existe antes de realizar operações sobre ele. Essa verificação é importante para evitar erros e falhas na execução do código. Neste post, vamos explorar como fazer essa verificação usando a linguagem de programação Rust.

## Como fazer

Para verificar se um diretório existe em Rust, utilizamos a função `metadata()` da biblioteca padrão `std::fs`. Esta função retorna um `Result` que, em caso de sucesso, contém informações sobre o diretório e, em caso de falha, retorna o erro correspondente.

Um exemplo de código que realiza essa verificação é o seguinte:

```Rust
use std::fs;

match fs::metadata("caminho/do/diretorio") {
    Ok(_) => println!("O diretório existe!"),
    Err(_) => println!("O diretório não existe!"),
}
```

No código acima, utilizamos a função `match` para tratar o resultado retornado pela função `metadata()`. Se a verificação for bem-sucedida, a mensagem "O diretório existe!" será impressa. Caso contrário, a mensagem "O diretório não existe!" será impressa.

## Deep Dive

Além da função `metadata()`, também é possível utilizar a função `exists()` da biblioteca `std::path`. Essa função retorna um valor booleano indicando se o caminho especificado existe ou não.

Outro ponto importante é que, ao verificar se um diretório existe, é possível também realizar operações sobre ele, como criar arquivos, renomeá-lo e movê-lo. Para isso, é recomendado utilizar a função `create_dir()` da biblioteca `std::fs`. É importante lembrar de sempre tratar possíveis erros ao realizar essas operações.

## Veja também

- [Documentação oficial sobre verificação de diretórios em Rust](https://doc.rust-lang.org/std/fs/fn.metadata.html)
- [Exemplos práticos de verificação de diretórios em Rust](https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=d3a6f10e6a9456849b77d44b57b6c939)