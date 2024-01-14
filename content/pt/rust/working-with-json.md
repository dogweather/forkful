---
title:                "Rust: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON em Rust?

JSON (JavaScript Object Notation) é um formato amplamente utilizado para representar e transmitir dados estruturados em diferentes plataformas e tecnologias. Ao trabalhar com Rust, uma linguagem de programação de sistema de alto desempenho, é importante ter conhecimento sobre como manipular dados JSON de forma eficiente e segura.

## Como fazer

Para começar a trabalhar com dados JSON em Rust, você precisará adicionar a biblioteca `serde` ao seu projeto. Isso pode ser feito adicionando a seguinte linha ao seu arquivo `Cargo.toml`:

```
[dependencies]
serde = "1.0"
```

Em seguida, na seção `[dependencies]` do seu arquivo `Cargo.toml`, adicione a biblioteca `serde_json` para lidar especificamente com o formato JSON:

```
serde_json = "1.0"
```

Agora, dentro do seu arquivo `main.rs`, importe essas bibliotecas usando a seguinte declaração:

```
use serde::{Deserialize, Serialize};
use serde_json::{Result, Value};
```

Para codificar um objeto em JSON, use o macro `serde_json::to_string` junto com a sua estrutura definida. Aqui está um exemplo:

```
#[derive(Serialize, Deserialize)]
struct User {
    name: String,
    age: u8,
    city: String,
}

fn main() -> Result<()> {
    let user = User {
        name: String::from("João"),
        age: 25,
        city: String::from("São Paulo"),
    };

    let user_json = serde_json::to_string(&user)?;

    println!("{}", user_json);

    Ok(())
}
```

Aqui, temos uma estrutura `User` que é anotada com os atributos `Serialize` e `Deserialize` fornecidos pela biblioteca `serde`. Isso nos permite converter a estrutura em uma representação JSON usando o método `to_string`.

A saída para esse exemplo seria:

```
{"name": "João", "age": 25, "city": "São Paulo"}
```

Para decodificar um objeto JSON, use o macro `serde_json::from_str` junto com sua estrutura definida. Aqui está um exemplo:

```
#[derive(Serialize, Deserialize)]
struct User {
    name: String,
    age: u8,
    city: String,
}

fn main() -> Result<()> {
    let user_json = r#"{"name": "Maria", "age": 30, "city": "Rio de Janeiro"}"#;

    let user: User = serde_json::from_str(user_json)?;

    println!("Name: {}, Age: {}, City: {}", user.name, user.age, user.city);

    Ok(())
}
```

Aqui, temos uma string representando um objeto JSON e usamos o método `from_str` para decodificar o objeto em uma estrutura `User`. A saída para esse exemplo seria:

```
Name: Maria, Age: 30, City: Rio de Janeiro
```

## Profundidade

Além da serialização e desserialização básica, a biblioteca `serde_json` também oferece recursos para manipulação mais avançada de dados JSON. Por exemplo, você pode usar os métodos `to_value` e `from_value` para converter dados em um tipo genérico `Value` que permite acessar e manipular objetos JSON de forma mais flexível.

Também é possível trabalhar com dados JSON aninhados e lidar com erros e casos especiais ao fazer a serialização e desserialização.

## Veja também

- Guia de início rápido do Rust para JSON (em inglês): https://serde.rs/json.html#getting-started
- Documentação completa da biblioteca `serde_json`: https://docs.serde.rs/serde_json/index.html
- Código fonte dos exemplos utilizados neste artigo: link para repositório no GitHub