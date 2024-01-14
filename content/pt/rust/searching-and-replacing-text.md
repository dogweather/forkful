---
title:                "Rust: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Rust"
category:             "Rust"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que utilizar a substituição de texto em Rust?

A substituição de texto é uma tarefa comum em muitos projetos de programação. Pode ser necessário alterar uma palavra específica em um documento ou realizar uma ação em massa em um grande conjunto de dados. Em Rust, é possível realizar essa tarefa de forma eficiente e eficaz usando as funcionalidades disponíveis na linguagem.

## Como fazer a substituição de texto em Rust?

Realizar a substituição de texto em Rust é um processo simples que pode ser facilmente implementado em qualquer projeto. Primeiro, é necessário importar a biblioteca "std::fs", que permite acessar e manipular arquivos. Em seguida, pode-se utilizar a função "read_to_string" para ler o conteúdo de um arquivo em uma variável. Com o conteúdo armazenado, a função "replace" pode ser usada para substituir uma palavra ou conjunto de caracteres específicos pelo desejado.

Um exemplo de código pode ser:

```Rust
use std::fs;

let mut file_contents = fs::read_to_string("arquivo.txt").expect("Erro ao ler o arquivo");

let new_contents = file_contents.replace("velho", "novo");

println!("{}", new_contents);
```

A saída desse código seria o conteúdo do arquivo com a substituição da palavra "velho" por "novo". É importante ressaltar que o conteúdo original do arquivo não é modificado, apenas é criada uma nova variável com a substituição realizada.

## Aprofundando na substituição de texto em Rust

Além da função "replace", Rust também possui outras opções para a substituição de texto. Por exemplo, é possível utilizar a função "regex::Regex" para realizar uma substituição com expressões regulares, o que permite uma maior flexibilidade e abrangência na manipulação de texto.

Também é importante mencionar que a função "replace" pode ser combinada com outras operações, como "trim" e "split", para realizar substituições mais complexas e específicas. Combinando essas funcionalidades, é possível criar algoritmos poderosos para a manipulação de texto em Rust.

## Veja também

- Documentação da biblioteca "std::fs" em Rust (em inglês): https://doc.rust-lang.org/std/fs/index.html
- Tutorial sobre expressões regulares em Rust (em inglês): https://docs.rs/regex/1.4.0/regex/
- Exemplos de uso da função "replace" em Rust (em inglês): https://www.educative.io/edpresso/how-to-replace-strings-in-rust