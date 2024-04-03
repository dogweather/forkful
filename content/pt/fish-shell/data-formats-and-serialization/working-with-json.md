---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:41.724625-07:00
description: "Como Fazer: O Fish Shell, por si s\xF3, n\xE3o possui utilidades internas\
  \ para an\xE1lise e gera\xE7\xE3o de JSON. No entanto, ele integra-se perfeitamente\
  \ com\u2026"
lastmod: '2024-03-13T22:44:47.026995-06:00'
model: gpt-4-0125-preview
summary: "O Fish Shell, por si s\xF3, n\xE3o possui utilidades internas para an\xE1\
  lise e gera\xE7\xE3o de JSON."
title: Trabalhando com JSON
weight: 38
---

## Como Fazer:
O Fish Shell, por si só, não possui utilidades internas para análise e geração de JSON. No entanto, ele integra-se perfeitamente com ferramentas de terceiros como o `jq` para processamento de JSON. O `jq` é um processador JSON de linha de comando poderoso e versátil que permite recortar, filtrar, mapear e transformar dados estruturados com uma linguagem simples e expressiva.

### Analisando JSON com jq
Para analisar um arquivo JSON e extrair dados usando `jq`:

```fish
# Supondo que você tenha um arquivo JSON chamado 'data.json' com o conteúdo: {"name":"Fish Shell","version":"3.4.0"}
cat data.json | jq '.name'
# Exemplo de saída
"Fish Shell"
```

### Gerando JSON com jq
Criando conteúdo JSON a partir de variáveis ou saídas do shell:

```fish
# Criar objeto JSON a partir de variáveis
set name "Fish Shell"
set version "3.4.0"
jq -n --arg name "$name" --arg version "$version" '{name: $name, version: $version}'
# Exemplo de saída
{
  "name": "Fish Shell",
  "version": "3.4.0"
}
```

### Filtrando Coleções JSON
Suponha que temos um array JSON de objetos em um arquivo chamado `versions.json`:
```json
[
  {"version": "3.1.2", "stable": true},
  {"version": "3.2.0", "stable": false},
  {"version": "3.4.0", "stable": true}
]
```
Para filtrar este array apenas pelas versões estáveis:

```fish
cat versions.json | jq '.[] | select(.stable == true) | .version'
# Exemplo de saída
"3.1.2"
"3.4.0"
```

Os exemplos fornecidos demonstram o poder de integrar o `jq` com o Fish Shell para operações com JSON. Aproveitar tais ferramentas enriquece a experiência do shell, tornando-o um ambiente formidável para lidar com formatos de dados modernos.
