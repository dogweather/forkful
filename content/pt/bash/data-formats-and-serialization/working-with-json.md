---
title:                "Trabalhando com JSON"
date:                  2024-02-03T19:21:37.711296-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que é & Por quê?
Trabalhar com JSON em programação Bash envolve analisar, extrair e manipular dados JSON diretamente da linha de comando. Programadores frequentemente fazem isso para integrar de forma transparente scripts shell com APIs web e formatos modernos de troca de dados, tornando a programação em Bash mais poderosa e relevante em um ecossistema pesado de JSON.

## Como fazer:
O Bash por si só não possui capacidades internas de análise de JSON, mas `jq` é um poderoso processador JSON de linha de comando que preenche essa lacuna. Veja como usá-lo:

**Lendo um arquivo JSON:**

Exemplo `data.json`:
```json
{
  "name": "Jane Doe",
  "email": "jane@example.com",
  "location": {
    "city": "New York",
    "country": "USA"
  }
}
```

Para ler e extrair o nome do arquivo JSON:
```bash
jq '.name' data.json
```
Saída:
```
"Jane Doe"
```

**Modificando dados JSON:**

Para atualizar a cidade para "Los Angeles" e escrever de volta no arquivo:
```bash
jq '.location.city = "Los Angeles"' data.json > temp.json && mv temp.json data.json
```

**Analisando JSON de uma variável:**

Se você tem JSON em uma variável Bash, `jq` ainda pode processá-lo:
```bash
json_string='{"name": "John Doe", "email": "john@example.com"}'
echo $json_string | jq '.name'
```
Saída:
```
"John Doe"
```

**Trabalhando com arrays:**

Dado um array de itens em JSON:
```json
{
  "items": ["apple", "banana", "cherry"]
}
```

Para extrair o segundo item (a indexação começa em 0):
```bash
jq '.items[1]' data.json
```
Saída:
```
"banana"
```

Para operações mais complexas e filtragem, `jq` tem um manual abrangente e tutoriais disponíveis online, tornando-o uma ferramenta versátil para todas as suas necessidades de Bash/JSON.