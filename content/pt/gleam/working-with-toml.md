---
title:                "Trabalhando com TOML"
date:                  2024-01-26T04:22:07.357833-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com TOML"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/gleam/working-with-toml.md"
---

{{< edit_this_page >}}

## O Quê & Porquê?
Trabalhar com TOML significa fazer o parsing e a geração de arquivos TOML (Tom's Obvious, Minimal Language) com código. Programadores usam TOML para arquivos de configuração de fácil leitura e serialização de dados, graças à sua clareza semântica e compatibilidade com tipos de dados convencionais.

## Como fazer:
Gleam não possui suporte embutido para TOML, então você precisará de uma biblioteca externa. Por exemplo:

```gleam
// Assumindo que você tenha uma biblioteca de parsing de TOML:
import toml/{Parser, Encoder}

// Fazer o parse do conteúdo TOML
let toml_content = """
[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

let parsed = Parser.parse(toml_content)

// Usar os dados parseados
match parsed {
  Ok(data) -> "Dados parseados com sucesso!"
  Error(_) -> "Falha ao parsear dados."
}

// Gerar conteúdo TOML a partir da estrutura de dados do Gleam
let data = #{
  "owner": #{
    "name": "Tom Preston-Werner",
    "dob": "1979-05-27T07:32:00Z"
  }
}

let toml_string = Encoder.encode(data)
```

Saída de exemplo:

```
Dados parseados com sucesso!
```

## Estudo Aprofundado
TOML foi lançado em 2013 por Tom Preston-Werner. Seu objetivo: ser mais legível e direto do que XML e menos complexo do que YAML para configurações de arquivos. Apesar da simplicidade, é robusto para dados estruturados, oferecendo uma sintaxe explícita e fácil de entender. Alternativas incluem JSON, YAML e INI, mas a sintaxe minimalista e clara do TOML muitas vezes prevalece para arquivos de configuração. Implementar TOML em Gleam envolve duas ações principais: fazer o parsing de TOML em estruturas de dados nativas e serializar estruturas de dados nativas em TOML. A maioria das bibliotecas TOML para Erlang ou Elixir pode ser usada em Gleam devido à sua interoperabilidade com os idiomas BEAM, garantindo uma integração perfeita dentro dos projetos Gleam.

## Veja Também
- Especificações da linguagem TOML: [https://toml.io/en/](https://toml.io/en/)
- Um parser TOML para Erlang: [https://hex.pm/packages/toml](https://hex.pm/packages/toml)
- TOML no GitHub: [https://github.com/toml-lang/toml](https://github.com/toml-lang/toml)
