---
title:                "Trabalhando com json"
html_title:           "Rust: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?

JSON (JavaScript Object Notation) é um formato amplamente utilizado para troca de dados entre sistemas e linguagens de programação. Ao trabalhar com JSON, você pode facilmente serializar e deserializar dados, o que é especialmente útil para comunicação entre aplicações cliente-servidor.

## Como fazer:

### Instalando a biblioteca `serde_json`
Para começar a trabalhar com JSON em Rust, você precisará instalar a biblioteca `serde_json` usando o gerenciador de pacotes `Cargo`. Basta executar o comando `cargo add serde_json` no seu terminal.

### Serializando dados em JSON
Para serializar uma estrutura de dados em JSON, primeiro você precisa marcá-la com a anotação `Serialize` do `serde`. Em seguida, use a função `to_string` da biblioteca `serde_json` para converter seu objeto em uma string JSON.

```rust
extern crate serde;
extern crate serde_json;

use serde::{Serialize};

#[derive(Serialize)]
struct Pessoa {
    id: u64,
    nome: String,
    idade: u8
}

fn main() {
    let pessoa = Pessoa {
        id: 1,
        nome: "Maria".to_string(),
        idade: 30
    };

    let json = serde_json::to_string(&pessoa).unwrap();

    println!("{}", json);
}
```

A saída desse código seria `{"id":1,"nome":"Maria","idade":30}`.

### Deserializando dados em JSON
Para deserializar dados em JSON, você precisa marcar sua estrutura de dados com a anotação `Deserialize` do `serde`. Em seguida, use a função `from_str` da biblioteca `serde_json` para converter uma string JSON em um objeto.

```rust
extern crate serde;
extern crate serde_json;

use serde::{Deserialize};

#[derive(Deserialize)]
struct Pessoa {
    id: u64,
    nome: String,
    idade: u8
}

fn main() {
    let json = r#"{"id":1,"nome":"Maria","idade":30}"#;

    let pessoa: Pessoa = serde_json::from_str(json).unwrap();

    println!("{} tem {} anos.", pessoa.nome, pessoa.idade);
}
```

A saída desse código seria `Maria tem 30 anos.`.

## Aprofundando-se:

Há uma série de outras funcionalidades e recursos disponíveis na biblioteca `serde_json` para trabalhar com JSON em Rust. Algumas delas são:

- Configurando opções de serialização e deserialização
- Serealização e deserialização de tipos genéricos
- Manipulação de erros durante a serialização e deserialização

Para saber mais sobre esses recursos e outros, confira a documentação oficial da biblioteca `serde_json`.

## Veja também:

- [Documentação oficial da biblioteca `serde_json`](https://docs.serde.rs/serde_json/)
- [Exemplos de uso da biblioteca `serde_json`](https://github.com/serde-rs/json/tree/master/examples)
- [Tutorial de JSON em Rust no Medium (em inglês)](https://medium.com/swlh/build-a-graphql-api-with-rust-and-juniper-part-1-b45b96b76862)