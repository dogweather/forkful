---
title:                "Trabalhando com JSON"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-json.md"
---

{{< edit_this_page >}}

## O Que é e Por Que?
Trabalhar com JSON significa manipular um formato de troca de dados bem comum na web. Programadores fazem isso para interagir com APIs, armazenar configurações e trocar informações entre sistemas de maneira compacta e legível.

## Como Fazer:
Para lidar com JSON no Bash, você pode usar o comando `jq`, que é uma ferramenta de linha de comando para processar JSON. Aqui está um exemplo de como ler um valor de um arquivo JSON:

```Bash
echo '{"nome": "João", "idade": 30}' | jq '.nome'
```
Saída:
```
"João"
```

Para escrever em um arquivo JSON, você poderia fazer assim:

```Bash
echo '{"nome": "João", "idade": 30}' | jq '.idade = 31' > novo_arquivo.json
```

Isso atualiza a `idade` para `31` e salva o JSON modificado em `novo_arquivo.json`.

## Análise Profunda:
O JSON (Javascript Object Notation) existe desde o início dos anos 2000, inspirado na notação de objeto JavaScript, mas é totalmente independente da linguagem. Alternativas incluem XML e YAML, mas o JSON prevalece em muitos casos devido à sua simplicidade e compatibilidade com a web. Ao trabalhar com JSON no Bash, muitas vezes tarefas simples se tornam complexas devido à natureza da linguagem. Portanto, ferramentas como `jq` surgiram para facilitar essa integração.

## Veja Também:
- Documentação oficial `jq`: [stedolan.github.io/jq](https://stedolan.github.io/jq/)
- JSON no W3Schools: [www.w3schools.com/js/js_json_intro.asp](https://www.w3schools.com/js/js_json_intro.asp)
- Comparação entre JSON, XML e YAML: [www.json.org/xml.html](https://www.json.org/xml.html)
