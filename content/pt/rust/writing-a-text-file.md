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

## Por que

Se você é um programador iniciante ou experiente, é sempre útil saber como escrever um arquivo de texto em qualquer linguagem de programação. Isso é especialmente útil em situações em que você precisa armazenar dados ou informações de forma persistente.

## Como fazer

Para escrever um arquivo de texto em Rust, siga estes passos simples:

1. Importe a biblioteca padrão de entrada/saída (`std::io`).
2. Crie um objeto do tipo `File` usando a função `File::create()`.
3. Use o método `write_all()` para escrever os dados no arquivo.

Aqui está um exemplo de código que escreve uma frase simples em um arquivo de texto chamado "example.txt":

```Rust
use std::io::{self, Write};

fn main() {
    let mut file = std::fs::File::create("example.txt")
        .expect("Falha ao criar o arquivo");

    file.write_all(b"Olá, mundo!").expect("Falha ao escrever no arquivo");
}
```

Se tudo der certo, você deve ver um novo arquivo "example.txt" no diretório onde o código está sendo executado, contendo a frase "Olá, mundo!"

## Mergulho profundo

Ao escrever um arquivo de texto em Rust, existem algumas coisas importantes a serem consideradas:

- Ao usar o método `write_all()`, os dados serão sobrescritos no arquivo existente, se houver. Se você quiser adicionar dados em vez de sobrescrever, use o método `write()`.

- Certifique-se de lidar com possíveis erros ao escrever no arquivo, usando o `expect()` ou `unwrap()` para lidar com os resultados da função.

- Se você quiser escrever dados formatados em vez de apenas uma sequência de bytes, use a macro `format!()` para criar uma string formatada e depois escreva-a no arquivo.

## Veja também

- [Documentação oficial do Rust sobre entrada/saída](https://doc.rust-lang.org/std/io/)
- [Artigo sobre leitura de arquivos em Rust](https://www.educative.io/edpresso/how-to-read-a-file-in-rust)