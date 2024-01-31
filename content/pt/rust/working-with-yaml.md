---
title:                "Trabalhando com YAML"
date:                  2024-01-19
simple_title:         "Trabalhando com YAML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que & Por Que?
YAML é um formato para serializar dados humanamente legíveis. Programadores usam YAML para configurações, arquivos de dados e em muitos casos em que JSON poderia ser usado, mas a legibilidade é prioridade.

## Como Fazer:

1. Adicionar dependência `serde` e `serde_yaml` ao `Cargo.toml`:
   ```toml
   [dependencies]
   serde = { version = "1.0", features = ["derive"] }
   serde_yaml = "0.8"
   ```

2. Definir uma estrutura e serializar para YAML:
   ```rust
   use serde::{Serialize, Deserialize};

   #[derive(Debug, Serialize, Deserialize)]
   struct Config {
       nome: String,
       habilitado: bool,
       itens: Vec<String>,
   }

   fn main() {
       let config = Config {
           nome: "Exemplo".into(),
           habilitado: true,
           itens: vec!["Um".into(), "Dois".into(), "Três".into()],
       };

       let yaml_string = serde_yaml::to_string(&config).unwrap();
       println!("{}", yaml_string);
   }
   ```
   Saída:
   ```yaml
   ---
   nome: Exemplo
   habilitado: true
   itens:
     - Um
     - Dois
     - Três
   ```

3. Deserializar YAML de volta para a estrutura:
   ```rust
   fn main() {
       let yaml = r#"
           nome: Exemplo
           habilitado: true
           itens:
             - Um
             - Dois
             - Três
       "#;

       let deserialized_config: Config = serde_yaml::from_str(yaml).unwrap();
       println!("{:?}", deserialized_config);
   }
   ```

   Saída:
   ```plaintext
   Config { nome: "Exemplo", habilitado: true, itens: ["Um", "Dois", "Três"] }
   ```

## Aprofundando:

YAML (YAML Ain't Markup Language) surgiu no início dos anos 2000 como uma alternativa mais legível ao XML. Opções como TOML e JSON também são usadas para finalidades semelhantes, mas YAML é especialmente apreciado pela sua facilidade de leitura e edição manual. No Rust, o serde_yaml tira proveito da biblioteca `serde` para serialização/deserialização, tornando-se a escolha de facto para trabalhar com YAML.

## Veja Também:

- Documentação Serde: https://serde.rs/
- Crates serde_yaml: https://crates.io/crates/serde_yaml
- YAML Oficial: https://yaml.org/
- Comparação entre JSON, YAML e TOML: https://blog.logrocket.com/json-vs-yaml-vs-toml-which-is-the-best-configuration-file-format-for-javascript-applications/
- Guia Serde: https://serde.rs/attributes.html
