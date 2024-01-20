---
title:                "Trabalhando com JSON"
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## O Que é & Por Que?

Trabalhar com JSON, que significa JavaScript Object Notation, é gerenciar dados em um formato leve de troca de dados. Programadores usam JSON porque é fácil de ler e escrever para humanos, e simples de analisar e gerar para máquinas.

## Como Fazer:

```Gleam
import gleam/json

pub fn main() {
  let dados = "{\"nome\": \"João\", \"idade\": 30}"
  let json = json.from_string(dados)
  
  case json {
    Ok(value) -> io.println(value)
    Error(error) -> io.println(error)
  }
}
```

Saída esperada:
```
Ok(#{"idade": 30, "nome": "João"})
```

## Mergulho Profundo:

JSON começou nos anos 2000 como uma forma de representar dados em JavaScript. Hoje, é um padrão em diversas linguagens de programação, incluindo Gleam, com sua biblioteca `gleam/json`. Alternativas ao JSON incluem XML e YAML, mas JSON se destaca por sua velocidade e simplicidade. No Gleam, você vai querer ler e decodificar JSON para tipos conhecidos, e a biblioteca oferece ferramentas para facilitar esse processo.

## Veja Também:

- Especificação do formato JSON: [Introducing JSON](https://www.json.org/json-en.html)