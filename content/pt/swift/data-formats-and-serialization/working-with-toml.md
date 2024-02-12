---
title:                "Trabalhando com TOML"
aliases: - /pt/swift/working-with-toml.md
date:                  2024-01-26T04:26:47.480196-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com TOML"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/swift/working-with-toml.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?
TOML (Tom's Obvious, Minimal Language - Linguagem Minimalista e Óbvia do Tom) é um formato de serialização de dados que é fácil de ler devido à sua clareza semântica. Programadores utilizam TOML para arquivos de configuração onde a legibilidade por humanos e a facilidade de análise por máquinas são essenciais.

## Como fazer:
Para começar, você precisa de um analisador de TOML. Swift não tem um embutido, então vamos utilizar o `TOMLDecoder`. Instale-o via Gerenciador de Pacotes Swift e então serialize e deserialize TOML com facilidade.

```Swift
import TOMLDecoder

let tomlString = """
title = "Exemplo de TOML"

[owner]
name = "Tom Preston-Werner"
dob = 1979-05-27T07:32:00Z
"""

struct Config: Codable {
    let title: String
    let owner: Owner
}

struct Owner: Codable {
    let name: String
    let dob: Date
}

let decoder = TOMLDecoder()
if let configData = tomlString.data(using: .utf8) {
    do {
        let config = try decoder.decode(Config.self, from: configData)
        print("Título: \(config.title), Proprietário: \(config.owner.name), Nascimento: \(config.owner.dob)")
    } catch {
        print("Erro ao analisar TOML: \(error)")
    }
}
```

Este código produz:
```
Título: Exemplo de TOML, Proprietário: Tom Preston-Werner, Nascimento: 1979-05-27 07:32:00 +0000
```

## Aprofundamento
TOML foi desenhado por Tom Preston-Werner, co-fundador do GitHub, como uma alternativa mais amigável ao ser humano do que formatos como JSON ou YAML. Visa a clareza, reduzindo as chances de má interpretação por um humano ou máquina. Quanto às alternativas, YAML e JSON são os suspeitos do costume, com YAML inclinado para a legibilidade humana e JSON como a opção mais simples amigo da máquina. Ao trabalhar com TOML em Swift, não temos um analisador nativo. No entanto, bibliotecas de terceiros como o `TOMLDecoder` facilitam a conversão fácil entre strings TOML e tipos Swift, especificamente via protocolos `Codable` introduzidos no Swift 4 que racionalizaram a serialização.

## Veja Também
- O padrão TOML: https://toml.io
- GitHub para `TOMLDecoder`: https://github.com/dduan/TOMLDecoder
- Documentação Swift sobre `Codable`: https://developer.apple.com/documentation/swift/codable
- Comparação de formatos de serialização de dados: https://pt.wikipedia.org/wiki/Comparação_de_formatos_de_serialização_de_dados
