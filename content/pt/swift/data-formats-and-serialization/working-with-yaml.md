---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:51.980729-07:00
description: "YAML, que significa YAML Ain't Markup Language (YAML N\xE3o \xE9 uma\
  \ Linguagem de Marca\xE7\xE3o), \xE9 um padr\xE3o de serializa\xE7\xE3o de dados\
  \ amig\xE1vel para humanos para\u2026"
lastmod: '2024-03-13T22:44:46.941236-06:00'
model: gpt-4-0125-preview
summary: "YAML, que significa YAML Ain't Markup Language (YAML N\xE3o \xE9 uma Linguagem\
  \ de Marca\xE7\xE3o), \xE9 um padr\xE3o de serializa\xE7\xE3o de dados amig\xE1\
  vel para humanos para\u2026"
title: Trabalhando com YAML
weight: 41
---

## O Quê & Por Quê?
YAML, que significa YAML Ain't Markup Language (YAML Não é uma Linguagem de Marcação), é um padrão de serialização de dados amigável para humanos para todas as linguagens de programação. Programadores o utilizam para arquivos de configuração, mensagens entre processos e armazenamento de dados porque sua legibilidade é muito mais próxima do inglês simples comparada a outros formatos de dados como XML ou JSON, tornando-o mais simples de entender e escrever.

## Como fazer:
Swift não inclui suporte integrado para análise (parsing) e serialização de YAML, necessitando o uso de bibliotecas de terceiros. Uma escolha popular é `Yams`, uma biblioteca para trabalhar com YAML em Swift.

Primeiro, você precisa adicionar `Yams` ao seu projeto. Se você está usando Swift Package Manager, pode adicioná-lo como uma dependência no seu arquivo `Package.swift`:

```swift
dependencies: [
    .package(url: "https://github.com/jpsim/Yams.git", de: "4.0.0")
]
```

### Analisando YAML em Swift
Assuma que você tenha a seguinte configuração YAML para um aplicativo simples:

```yaml
nome: MeuApp
versão: 1.0
ambiente: desenvolvimento
funcionalidades:
  - login
  - notificações
```

Veja como você pode analisar essa string YAML em Swift usando `Yams`:

```swift
import Yams

let yamlString = """
nome: MeuApp
versão: 1.0
ambiente: desenvolvimento
funcionalidades:
  - login
  - notificações
"""

do {
    if let data = try Yams.load(yaml: yamlString) as? [String: Any] {
        print(data)
        // Exemplo de acesso aos dados analisados
        if let nome = data["nome"] as? String {
            print("Nome do Aplicativo: \(nome)")
        }
    }
} catch {
    print("Erro ao analisar YAML: \(error)")
}
```

Saída de exemplo:

```
["nome": MeuApp, "versão": 1.0, "ambiente": "desenvolvimento", "funcionalidades": ["login", "notificações"]]
Nome do Aplicativo: MeuApp
```

### Serializando Objetos Swift para YAML
Converter um objeto Swift de volta para uma string YAML também é simples com `Yams`. Suponha que você tenha a mesma estrutura de dados que precisa ser serializada:

```swift
let infoApp = [
    "nome": "MeuApp",
    "versão": 1.0,
    "ambiente": "desenvolvimento",
    "funcionalidades": ["login", "notificações"]
] as [String : Any]

do {
    let yamlString = try Yams.dump(objeto: infoApp)
    print(yamlString)
} catch {
    print("Erro ao serializar para YAML: \(error)")
}
```

Isso produzirá uma String formatada em YAML:

```yaml
ambiente: desenvolvimento
funcionalidades:
  - login
  - notificações
nome: MeuApp
versão: 1.0
```

Estes exemplos demonstram operações básicas para trabalhar com YAML em aplicações Swift. Lembre-se, embora YAML se destaque em legibilidade humana e facilidade de uso, sempre considere as necessidades específicas da sua aplicação, especialmente no que diz respeito a desempenho e complexidade, ao escolher seu formato de serialização de dados.
