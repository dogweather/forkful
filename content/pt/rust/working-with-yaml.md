---
title:                "Rust: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML em Rust?

Trabalhar com YAML em Rust pode oferecer muitas vantagens para desenvolvedores. O YAML é uma linguagem de marcação que permite a criação de arquivos de configuração legíveis para humanos, o que torna a manutenção desses arquivos muito mais fácil. Além disso, trabalhar com YAML em uma linguagem de programação poderosa como Rust permite a criação de ferramentas mais robustas e eficientes para lidar com arquivos de configuração.

## Como fazer?

Para começar a trabalhar com YAML em Rust, primeiro precisamos adicionar a dependência do crate "yaml-rust" ao nosso projeto. Em seguida, podemos usar as funções e macros desse crate para ler, escrever e manipular arquivos YAML.

Um exemplo de código para ler um arquivo YAML e imprimir seu conteúdo seria o seguinte:

```rust
use std::fs::File;
use yaml_rust::{YamlLoader, Yaml};

fn main() {
    let file = File::open("arquivo.yml").unwrap();
    let dados = YamlLoader::load_from_reader(file).unwrap();
    let dados = &dados[0];
    println!("{:#?}", dados);
}
```

A saída desse código seria algo como:

```rust
YamlMapping(
    [
        (
            YamlString(
                "nome",
            ),
            YamlString(
                "João",
            ),
        ),
        (
            YamlString(
                "idade",
            ),
            YamlInteger(
                25,
            ),
        ),
        (
            YamlString(
                "hobbies",
            ),
            YamlArray(
                [
                    YamlString(
                        "programação",
                    ),
                    YamlString(
                        "jogos",
                    ),
                    YamlString(
                        "fotografia",
                    ),
                ],
            ),
        ),
    ],
)
```

Este é apenas um exemplo básico, mas existem muitas outras funções e macros disponíveis para manipular arquivos YAML em Rust.

## Mergulho profundo

Para aqueles que desejam se aprofundar em trabalhar com YAML em Rust, existem vários recursos disponíveis. O livro "The Rust Programming Language" tem um capítulo dedicado a trabalhar com YAML, além disso, a documentação oficial do crate "yaml-rust" também oferece muitas informações e exemplos.

Além disso, também é possível encontrar muitos projetos de código aberto em Rust que utilizam YAML para seus arquivos de configuração. Isso pode ser uma ótima maneira de ver como outros desenvolvedores estão lidando com o YAML em Rust e aprender com seus códigos.

## Veja também

- The Rust Programming Language: https://doc.rust-lang.org/book/

- Documentação do crate "yaml-rust": https://docs.rs/yaml-rust/

- Exemplo de projeto de código aberto em Rust com arquivos YAML: https://github.com/nushell/nushell/