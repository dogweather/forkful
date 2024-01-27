---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Quê & Por Quê?
Trabalhar com YAML é lidar com uma linguagem de serialização de dados legível para humanos, comum para configurações e troca de dados. Programadores usam YAML por sua simplicidade e facilidade de leitura/escrever comparado a JSON ou XML.

## Como fazer:
```gleam
// Atualmente, Gleam não tem bibliotecas padrão para YAML.
// Você precisaria usar uma biblioteca externa ou interagir com código Erlang/OTP.
```
Exemplo hipotético (Gleam ainda não tem suporte direto para YAML):
```gleam
import yaml

fn main() {
  let data = """
  - Hopper
  - Lovelace
  - Turing
  """
  let list = yaml.decode(data)
  case list {
    Ok(names) -> names |> List.map(fn(name) { name ++ " é incrível!" })
    Error(_) -> ["Erro ao decodificar YAML"]
  }
}
```
Saída hipotética:
```
["Hopper é incrível!", "Lovelace é incrível!", "Turing é incrível!"]
```

## Aprofundamento
YAML surgiu em 2001, idealizado como uma alternativa mais fácil de usar em comparação com XML para serialização de dados. Alternativas incluem JSON, XML e TOML. YAML é normalmente usado em arquivos de configuração e para trocar dados entre diferentes linguagens de programação, pois é facilmente interpretado por humanos e máquinas.

## Veja Também
- [Documentação Oficial YAML](https://yaml.org/)
- [Wikipedia YAML](https://pt.wikipedia.org/wiki/YAML)
- Tutoriais de YAML em várias linguagens de programação.
- Informações sobre interop com Erlang para uso de bibliotecas YAML em Gleam.
