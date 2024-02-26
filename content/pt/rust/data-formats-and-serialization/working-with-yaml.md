---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:37.761230-07:00
description: "Na programa\xE7\xE3o com Rust, trabalhar com YAML (YAML Ain't Markup\
  \ Language) refere-se a analisar e gerar dados no formato YAML, um padr\xE3o de\
  \ serializa\xE7\xE3o de\u2026"
lastmod: '2024-02-25T18:49:44.008821-07:00'
model: gpt-4-0125-preview
summary: "Na programa\xE7\xE3o com Rust, trabalhar com YAML (YAML Ain't Markup Language)\
  \ refere-se a analisar e gerar dados no formato YAML, um padr\xE3o de serializa\xE7\
  \xE3o de\u2026"
title: Trabalhando com YAML
---

{{< edit_this_page >}}

## O Que & Por Quê?

Na programação com Rust, trabalhar com YAML (YAML Ain't Markup Language) refere-se a analisar e gerar dados no formato YAML, um padrão de serialização de dados amigável para humanos. Os programadores integram o manuseio do YAML em Rust para configurar aplicações, gerenciar configurações ou processar estruturas de dados complexas de forma clara e legível, aproveitando sua simplicidade em relação ao JSON ou XML para arquivos de configuração e troca de dados.

## Como fazer:

Rust não suporta YAML em sua biblioteca padrão, então comumente usamos crates de terceiros como `serde` (para serializar e desserializar dados) em combinação com `serde_yaml`.

Primeiro, adicione dependências ao seu `Cargo.toml`:

```toml
[dependencies]
serde = { version = "1.0", features = ["derive"] }
serde_yaml = "0.8"
```

Agora, vamos ver como desserializar uma string YAML em uma struct de Rust e serializar uma struct de Rust de volta para uma string YAML.

### Desserializando YAML em Estruturas Rust

Defina uma struct Rust que espelhe os dados que você espera em YAML. Use atributos Serde para personalização, se necessário.

```rust
use serde::{Deserialize, Serialize};
use serde_yaml;

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Config {
    name: String,
    durability: i32,
    owner: Owner,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct Owner {
    name: String,
    age: i32,
}

fn main() {
    let yaml_data = "
name: Shield
durability: 300
owner:
  name: Steve
  age: 25
";

    let deserialized_config: Config = serde_yaml::from_str(yaml_data).unwrap();
    println!("{:?}", deserialized_config);
}
```

A saída de exemplo ao executar o código Rust acima seria:

```plaintext
Config { name: "Shield", durability: 300, owner: Owner { name: "Steve", age: 25 } }
```

### Serializando Estruturas Rust em YAML

Este exemplo pega a struct `Config` da seção anterior e a serializa de volta para o formato YAML.

```rust
fn main() {
    let config = Config {
        name: String::from("Axe"),
        durability: 120,
        owner: Owner {
            name: String::from("Alex"),
            age: 30,
        },
    };

    let serialized_yaml = serde_yaml::to_string(&config).unwrap();
    println!("{}", serialized_yaml);
}
```

A saída esperada será uma string formatada em YAML:

```yaml
---
name: Axe
durability: 120
owner:
  name: Alex
  age: 30
```

Estes trechos demonstram como integrar de forma eficiente a análise e geração de YAML em suas aplicações Rust, utilizando os populares crates `serde` e `serde_yaml`, acomodando estruturas de dados complexas e fornecendo configurações simples e legíveis para humanos.
