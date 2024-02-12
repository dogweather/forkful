---
title:                "Trabalhando com JSON"
aliases:
- /pt/rust/working-with-json.md
date:                  2024-02-03T19:24:01.629026-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Quê & Por Quê?

Trabalhar com JSON (JavaScript Object Notation) em Rust consiste em analisar dados JSON para estruturas de dados do Rust e serializar estruturas de dados do Rust de volta para JSON. Programadores fazem isso para interagir com APIs web, arquivos de configuração, ou qualquer formato de troca de dados onde o JSON é utilizado devido ao seu formato leve e legível por humanos.

## Como fazer:

Para trabalhar com JSON em Rust, a crate `serde` junto com `serde_json` para serialização e deserialização são extensivamente utilizadas. Primeiro, certifique-se de incluí-los em seu `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"
```

### Exemplo 1: Deserializar JSON para uma Struct Rust

Defina uma struct Rust e use macros derive para `Deserialize` e `Serialize`:

```rust
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct User {
    id: u32,
    name: String,
    email: String,
}

fn main() {
    let json_data = r#"
        {
            "id": 1,
            "name": "Jane Doe",
            "email": "jane.doe@example.com"
        }
    "#;

    let user: User = serde_json::from_str(json_data).unwrap();

    println!("ID do Usuário: {}", user.id);
    println!("Nome do Usuário: {}", user.name);
    println!("Email do Usuário: {}", user.email);
}
```

**Saída:**

```
ID do Usuário: 1
Nome do Usuário: Jane Doe
Email do Usuário: jane.doe@example.com
```

### Exemplo 2: Serializar uma Struct Rust para JSON

Usando a mesma struct `User`:

```rust
let user = User {
    id: 1,
    name: "Jane Doe".to_string(),
    email: "jane.doe@example.com".to_string(),
};

let json_data = serde_json::to_string(&user).unwrap();

println!("{}", json_data);
```

**Saída:**

```json
{"id":1,"name":"Jane Doe","email":"jane.doe@example.com"}
```

Esses exemplos demonstram o fluxo básico de deserializar JSON em estruturas Rust e serializar estruturas Rust de volta em strings JSON. O Serde fornece um rico conjunto de ferramentas para trabalhar com JSON, incluindo o tratamento de campos opcionais, aninhamento complexo e tipos não diretamente suportados pelo JSON.
