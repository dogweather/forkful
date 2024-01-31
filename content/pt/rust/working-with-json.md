---
title:                "Trabalhando com JSON"
date:                  2024-01-19
simple_title:         "Trabalhando com JSON"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-json.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?
Trabalhar com JSON (JavaScript Object Notation) é essencial para trocar dados de forma simples e organizada entre servidores e clientes web. Programadores fazem isso devido ao formato ser leve, fácil de ler e escrever para humanos, além de ser simples de analisar e gerar por máquinas.

## Como Fazer:
```Rust
use serde::{Serialize, Deserialize};
use serde_json;

// Definição de uma estrutura de dados para serializar/deserializar
#[derive(Serialize, Deserialize, Debug)]
struct Pessoa {
    nome: String,
    idade: u8,
    email: String,
}

fn main() {
    // Criar um objeto Pessoa
    let pessoa = Pessoa {
        nome: String::from("João"),
        idade: 30,
        email: String::from("joao@email.com"),
    };

    // Serializar o objeto Pessoa para uma string JSON
    let json_string = serde_json::to_string(&pessoa).unwrap();
    println!("JSON serializado: {}", json_string);

    // Deserializar a string JSON para um objeto Pessoa
    let pessoa_deserializada: Pessoa = serde_json::from_str(&json_string).unwrap();
    println!("Pessoa deserializada: {:?}", pessoa_deserializada);
}
```
Saída:
```
JSON serializado: {"nome":"João","idade":30,"email":"joao@email.com"}
Pessoa deserializada: Pessoa { nome: "João", idade: 30, email: "joao@email.com" }
```

## Mergulho Profundo
JSON surgiu no início dos anos 2000, projetado por Douglas Crockford, facilitando a troca de dados entre o cliente e o servidor, independente de linguagens de programação. Alternativas ao JSON incluem XML e YAML, mas o JSON destaca-se por ser mais enxuto e rápido de processar. Em Rust, a biblioteca `serde_json` é amplamente adotada para implementar a serialização e deserialização de JSON, oferecendo uma integração eficiente e segura de tipos graças ao sistema de tipos do Rust e a traits `Serialize` e `Deserialize`.

## Veja Também
- Documentação oficial da `serde_json`: https://docs.serde.rs/serde_json/
- Livro Oficial "The Rust Programming Language" sobre como trabalhar com JSON: https://doc.rust-lang.org/book/ch20-00-final-project-a-web-server.html
- Tutorial sobre Rust e JSON com Serde: https://serde.rs/
- JSON na MDN Web Docs: https://developer.mozilla.org/pt-BR/docs/Learn/JavaScript/Objects/JSON
