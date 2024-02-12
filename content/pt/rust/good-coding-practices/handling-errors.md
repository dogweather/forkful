---
title:                "Tratamento de erros"
aliases: - /pt/rust/handling-errors.md
date:                  2024-01-26T00:57:51.279238-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tratamento de erros"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/handling-errors.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Tratamento de erros é sobre lidar com as coisas quando elas não seguem conforme o planejado. Programadores fazem isso para lidar com o inesperado, garantindo que seus programas em Rust sejam robustos e não simplesmente travem ao enfrentar um problema.

## Como fazer:

Rust lida com erros de duas formas principais: erros recuperáveis e irrecuperáveis. Vamos ver ambos.

Erros recuperáveis usam `Result<T, E>`:

```Rust
use std::fs::File;

fn open_file(filename: &str) -> Result<File, std::io::Error> {
    let f = File::open(filename);
    
    match f {
        Ok(file) => Ok(file),
        Err(e) => Err(e),
    }
}

fn main() {
    match open_file("hello.txt") {
        Ok(_file) => println!("Arquivo aberto com sucesso."),
        Err(_e) => println!("Falha ao abrir arquivo."),
    }
}
```

A saída pode ser "Arquivo aberto com sucesso." ou "Falha ao abrir arquivo.", dependendo do seu `hello.txt`.

Para erros irrecuperáveis, usamos `panic!`:

```Rust
fn main() {
    // Isso causará pânico no programa porque o arquivo provavelmente não existe.
    let _f = File::open("nowhere.txt").unwrap();
}
```

Execute e você verá uma mensagem de pânico. Seu programa para imediatamente.

## Mergulho Profundo

Historicamente, o tratamento de erros na programação tem sido uma bagunça. Rust acerta ao fazer uma distinção clara entre erros recuperáveis e irrecuperáveis.

O `Result` enum é para erros recuperáveis. É explícito - você lida com a variante `Ok` ou `Err`. Você tem métodos como `unwrap()` e `expect()` também, mas são atalhos rápidos e sujos que podem levar a um `panic!`.

`panic!` é a forma de Rust gritar que algo muito ruim aconteceu, e não consegue lidar com isso. É como um erro irrecuperável que para a execução imediatamente. Um pânico em Rust é frequentemente sentido com bugs que você não espera lidar, como indexação fora dos limites de um array.

Tratar erros retornando `Result` é preferido quando se espera lidar com erros. É o idiomático Rust, o que significa que é a maneira como os desenvolvedores de Rust concordaram em fazer as coisas. Existe o `Option<T>` também, para casos em que um erro é apenas algo sendo `None` ao invés de `Some(T)`. É tudo sobre esperar o inesperado sem medo.

Alternativas? Claro, você poderia usar outros pacotes de tratamento de erros para mais funcionalidades ou uso ergonômico. Como `anyhow` para tratamento de erros simples, ou `thiserror` para erros em código de biblioteca.

## Veja Também

Interessado em explorar mais a fundo? Aqui está por onde ir:

- [Livro Rust sobre Tratamento de Erros](https://doc.rust-lang.org/book/ch09-00-error-handling.html) - Ótimo lugar para entender a filosofia de tratamento de erros do Rust.
- [Rust por Exemplo: Tratamento de Erros](https://doc.rust-lang.org/rust-by-example/error.html) - Exemplos interativos para colocar a mão na massa.

Lembre-se, um bom tratamento de erros não é apenas programar; é cuidar dos usuários do seu código. Feliz codificação!
