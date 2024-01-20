---
title:                "Trabalhando com JSON"
html_title:           "Rust: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-json.md"
---

{{< edit_this_page >}}

# Trabalhando com JSON em Rust

## O que é e por que os programadores utilizam

JSON (JavaScript Object Notation) é uma linguagem de formatação de dados amplamente utilizada por programadores em diversas linguagens de programação. Ele é usado principalmente para transmitir dados entre um servidor e um cliente em aplicações web, mas também pode ser útil em outras áreas da programação.

Os programadores usam JSON por ser uma linguagem simples e fácil de entender, além de ser compatível com muitas linguagens de programação. Ele também oferece uma maneira conveniente de estruturar dados de forma legível e fácil de interpretar, tornando a comunicação de dados entre diferentes plataformas mais eficiente.

## Como fazer:

```Rust
# Importando a biblioteca JSON
use rustc_serialize::json;
# Criando um objeto JSON
let object = json::Json::from("{'nome': 'Maria', 'idade': 30, 'cidade': 'São Paulo'}");
# Acessando valores do objeto
let nome = object["nome"].as_string().unwrap();
# Imprimindo o valor do nome
println!("O nome é {}", nome);
```

Output: 
```
O nome é Maria
```

## Explorando mais a fundo:

Para entender melhor como trabalhar com JSON em Rust, é importante conhecer sua história e possíveis alternativas. JSON foi criado em 1999 por Douglas Crockford e se tornou um padrão popular na comunicação de dados na web. Alternativas a JSON incluem XML (Extensible Markup Language) e YAML (YAML Ain't Markup Language), mas JSON é geralmente considerado mais simples e fácil de utilizar.

Em Rust, a biblioteca padrão que oferece suporte a JSON é a rustc_serialize. No entanto, há outras bibliotecas disponíveis, como serde_json, que fornecem recursos extras e podem atender melhor às necessidades do projeto em questão.

## Veja também:

- [Um tutorial sobre JSON e Rust](https://medium.com/real-world-rust/build-a-json-deserializer-in-rust-1ff12e3993d7)