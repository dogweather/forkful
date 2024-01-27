---
title:                "Trabalhando com YAML"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com YAML"
simple_title:         "Trabalhando com YAML"
programming_language: "Bash"
category:             "Bash"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/bash/working-with-yaml.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
YAML significa "YAML Ain't Markup Language" (em um exemplo clássico de recursão em acrônimos), é um formato utilizado para configuração e troca de dados de forma legível. Programadores utilizam YAML devido à sua simplicidade, facilidade de leitura e suporte em diversas linguagens de programação.

## Como fazer:
Para trabalhar com YAML no Bash, é comum usar ferramentas como `yq`, que é semelhante ao `jq` para JSON. Instale o `yq` com:

```Bash
sudo wget https://github.com/mikefarah/yq/releases/download/v4.14.1/yq_linux_amd64 -O /usr/bin/yq && sudo chmod +x /usr/bin/yq
```

Ler um valor específico (supondo que `config.yaml` tenha `chave: valor`):

```Bash
yq eval '.chave' config.yaml
```

Saída:

```
valor
```

Escrever um novo valor:

```Bash
yq eval '.chave = "novoValor"' -i config.yaml
```

Convertendo YAML para JSON:

```Bash
yq eval -o=json config.yaml
```

## Aprofundando:
YAML foi criado em 2001 e é uma alternativa mais legível ao XML e JSON, embora tenha uma capacidade inferior de mapeamento de estruturas complexas comparado ao XML. Alternativas ao `yq` incluem scripts em linguagens como Python com a biblioteca `PyYAML`, ou Ruby com `yaml`. Ao trabalhar com YAML, é importante ser cuidadoso com indentação, pois é isso que define a estrutura.

## Veja também:
- Documentação oficial YAML: https://yaml.org/spec/1.2/spec.html
- Repositório GitHub `yq`: https://github.com/mikefarah/yq
- Tutorial YAML: https://learnxinyminutes.com/docs/yaml/
