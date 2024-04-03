---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:21:37.711296-07:00
description: "Trabalhar com JSON em programa\xE7\xE3o Bash envolve analisar, extrair\
  \ e manipular dados JSON diretamente da linha de comando. Programadores frequentemente\u2026"
lastmod: '2024-03-13T22:44:46.774631-06:00'
model: gpt-4-0125-preview
summary: "Trabalhar com JSON em programa\xE7\xE3o Bash envolve analisar, extrair e\
  \ manipular dados JSON diretamente da linha de comando."
title: Trabalhando com JSON
weight: 38
---

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
