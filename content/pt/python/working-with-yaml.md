---
title:                "Trabalhando com yaml"
html_title:           "Python: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## O que é e por que fazemos isso?
Trabalhar com YAML é uma forma de armazenar e representar dados em formato de texto simples. Os programadores usam YAML como uma alternativa legível por humanos para arquivos de configuração, especialmente em aplicações web.

## Como fazer:
Para começar a trabalhar com YAML em Python, primeiro importamos o módulo pyyaml, que nos permite ler e escrever arquivos YAML. Para ler um arquivo YAML existente, usamos a função load() e para salvar dados em um arquivo YAML, usamos a função dump(). Veja os exemplos abaixo:

```Python
# Importando o módulo pyyaml
import yaml

# Lendo um arquivo YAML existente
dados = yaml.load(open("arquivo.yaml", "r"))

# Salvando dados em um arquivo YAML
yaml.dump(dados, open("arquivo.yaml", "w"))
```

## Mergulhando mais fundo:
O formato YAML foi criado em 2001 por Clark Evans, Ingy döt Net e Oren Ben-Kiki como uma alternativa mais legível por humanos para linguagens de marcação como XML. Além disso, a formatação simples e compacta do YAML o torna muito popular em cenários de configuração de servidores e manipulação de arquivos de dados.

Alguns outros formatos populares de armazenamento de dados em formato de texto incluem JSON e CSV. No entanto, YAML oferece um suporte mais completo à estrutura de dados, permitindo que diferentes tipos de dados (como strings, listas e dicionários) sejam facilmente armazenados e acessados em um único arquivo.

## Veja também:
- [Documentação oficial do módulo pyyaml](https://pyyaml.org/wiki/PyYAMLDocumentation)