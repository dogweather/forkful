---
title:    "Rust: Criando um arquivo temporário."
keywords: ["Rust"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/rust/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Rust?

Criar um arquivo temporário é uma tarefa comum em programação, independentemente da linguagem utilizada. Em Rust, isso pode ser necessário para várias finalidades, como armazenar dados temporariamente, evitar conflitos entre processos ou realizar testes. Neste artigo, vamos ver como criar um arquivo temporário de forma simples e eficiente em Rust.

## Como criar um arquivo temporário em Rust

Para criar um arquivo temporário em Rust, podemos utilizar a função `tempfile::tempfile` do módulo `std::fs`. Esta função criará um arquivo temporário vazio no sistema de arquivos padrão e retornará seu handle em forma de `std::fs::File`. Vejamos um exemplo de como utilizá-la:

```
use std::fs::File;
use std::io::prelude::*;
use tempfile::tempfile;

fn main() {
    let mut file = tempfile().unwrap();
    file.write_all(b"Olá mundo!").unwrap();
}
```

Neste exemplo, estamos criando um arquivo temporário e escrevendo a string "Olá mundo!" nele. É importante lembrar que, após finalizarmos o uso do arquivo, devemos excluí-lo manualmente através da função `std::fs::remove_file`.

## Mergulho profundo

Existem várias opções que podemos passar para a função `tempfile::tempfile` para customizar nosso arquivo temporário. Algumas delas são:

- `prefix`: podemos passar um prefixo para o nome do arquivo temporário, o que pode ser útil para identificar a utilidade do arquivo.
- `suffix`: assim como o prefixo, podemos adicionar uma extensão para o nome do arquivo temporário.
- `suffix_len`: podemos especificar o tamanho do sufixo, caso seja necessário utilizar um número maior de caracteres.
- `tempfile`: é possível utilizar a função `tempfile` várias vezes para criar múltiplos arquivos temporários no mesmo código.

Além disso, o módulo `tempfile` também oferece outras funções úteis, como `tempdir`, que cria um diretório temporário, e `persist`, que converte um arquivo temporário em um arquivo permanente. Consulte a documentação oficial para mais informações.

## Veja também

- [Documentação do módulo `std::fs`] (https://doc.rust-lang.org/std/fs/index.html)
- [Documentação do pacote `tempfile`] (https://docs.rs/tempfile/3.1.0/tempfile/)
- [Exemplo de utilização da função `tempfile`] (https://play.rust-lang.org/?version=stable&mode=debug&edition=2018&gist=4e77fb3c8ce6cc6c1422f4b2f9dfd5b6)