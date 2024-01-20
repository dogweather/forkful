---
title:                "Trabalhando com yaml"
html_title:           "Rust: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Rust"
category:             "Rust"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/rust/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que e por que?

Trabalhar com YAML é uma maneira fácil de armazenar e transmitir dados estruturados em um formato legível por humanos. Os programadores geralmente usam YAML para configurar seus projetos ou definir configurações e opções para seus softwares.

## Como fazer:

### Instalando o pacote YAML

 Para começar a trabalhar com YAML em Rust, você precisa primeiro instalar o pacote `yaml-rust`. Para fazer isso, basta adicionar a seguinte linha ao seu arquivo `Cargo.toml`:

```Rust
yaml-rust = "0.4.3"
```

### Importando o módulo YAML

Agora que você tem o pacote instalado, é preciso importar o módulo YAML em seu código:

```Rust
use yaml_rust::{YamlLoader, YamlEmitter};
```

### Carregando um arquivo YAML

Para carregar um arquivo YAML em seu programa, você pode usar a função `load_from_str` do módulo YAML:

```Rust
let yaml_string = "nome: João\nidade: 25".to_owned();
let docs = YamlLoader::load_from_str(&yaml_string).unwrap();
```

### Lendo dados do YAML

Após carregar o arquivo YAML, você pode ler os dados armazenados nele usando os métodos `as_str`, `as_i64`, `as_bool`, entre outros. Por exemplo, se quisermos ler a idade do usuário no exemplo acima, podemos usar o seguinte código:

```Rust
let idade = docs[0]["idade"].as_i64().unwrap();
```

### Escrevendo um arquivo YAML

Para escrever um arquivo YAML a partir de um objeto em seu programa, você pode usar a função `dump` do módulo YAML:

```Rust
let user = vec![
    Yaml::String("nome".to_owned()),
    Yaml::String("João".to_owned()),
    Yaml::String("idade".to_owned()),
    Yaml::Integer(25),
];
let mut out_str = String::new();
let mut emitter = YamlEmitter::new(&mut out_str);
emitter.dump(&Yaml::Array(user)).unwrap();
```

## Aprofundando:

Existem muitas outras bibliotecas e ferramentas disponíveis para trabalhar com YAML em Rust, como `serde_yaml` e `yaml-rs`. Além disso, YAML tem sua própria especificação e histórico, que podem ser interessantes de explorar.

## Veja também:

- [Especificação YAML](https://yaml.org/spec/)