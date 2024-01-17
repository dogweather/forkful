---
title:                "Trabalhando com yaml"
html_title:           "Swift: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Swift"
category:             "Swift"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-yaml.md"
---

{{< edit_this_page >}}

# O que é e por que usar YAML?

YAML é uma linguagem de serialização de dados baseada em texto que é usada para representar dados estruturados em um formato fácil de ler e escrever. Ela se tornou popular entre os programadores por sua simplicidade e flexibilidade na criação de configurações e arquivos de dados.

# Como fazer:

Para começar a trabalhar com YAML em seu código Swift, primeiro é necessário importar a biblioteca YAML. Em seguida, você pode utilizar a função `load` para carregar um arquivo YAML em um objeto do tipo `Any?` que pode ser usado para acessar os dados estruturados dentro dele.

```Swift
import YAML

let data = try YAML.load("config.yml")
```

Você também pode usar a função `dump` para converter um objeto em uma string no formato YAML para salvar em um arquivo ou enviar em uma requisição.

```Swift
let config = [
    "username": "juliana",
    "password": "1234"
]
let yamlString = try YAML.dump(config)
```

# Profundando:

YAML foi criado em 2001 por Clark Evans e Ingy döt Net como uma alternativa mais amigável para o formato XML. No entanto, seu uso se popularizou principalmente em aplicações web e sistemas de configuração. Existem outras linguagens de serialização de dados, como JSON e XML, mas YAML é geralmente considerado como o mais legível e fácil de usar entre elas.

A biblioteca YAML para Swift é baseada na linguagem de C e é usada para análises e serializações YAML. É compatível com as versões mais recentes de Swift e é mantida pela comunidade do Swift.

# Veja também:

- Página oficial do YAML: https://yaml.org/
- Documentação da biblioteca YAML para Swift: https://github.com/aciidb0mb3r/YamlSwift