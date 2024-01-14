---
title:                "Python: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/python/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML em Python?

YAML é uma linguagem de marcação de dados simples e legível por humanos que pode ser usada para configurar ou armazenar dados estruturados. Em Python, trabalhar com YAML pode facilitar a leitura e escrita de dados de configuração ou criar estruturas de dados complexas de forma eficiente.

## Como trabalhar com YAML em Python

Para começar, é necessário importar a biblioteca PyYAML, que permite a leitura e escrita de dados em formato YAML em Python. Você pode instalar essa biblioteca através do gerenciador de pacotes PIP ou Anaconda. Em seguida, veja alguns exemplos de código e as respectivas saídas:

```
import yaml

# Lendo um arquivo YAML
with open("dados.yaml", "r") as arquivo:
    dados = yaml.safe_load(arquivo)

# Escrevendo um arquivo YAML
estrutura = [
    {
        "nome": "Maria",
        "idade": 30,
        "hobbies": ["leitura", "viagens"]
    },
    {
        "nome": "João",
        "idade": 25,
        "hobbies": ["futebol", "música"]
    }
]

with open("pessoas.yaml", "w") as arquivo:
    yaml.safe_dump(estrutura, arquivo)

# Converte uma string YAML em um dicionário
dados_string = """
nome: Ana
idade: 35
hobbies:
    - fotografia
    - culinária
"""

dados = yaml.safe_load(dados_string)
```

Aqui estão alguns exemplos básicos de como ler e escrever dados em formato YAML em Python. Além disso, você pode usar as estruturas de dados padrão do Python, como listas e dicionários, para armazenar dados YAML de forma eficiente.

## Mergulhando mais fundo em YAML em Python

YAML possui uma ampla gama de recursos e opções que podem ser exploradas. Por exemplo, é possível trabalhar com dados YAML multidocumento, aninhando estruturas de dados ou definindo referências. Além disso, você também pode personalizar a forma como os dados YAML são carregados e salvos em Python usando as opções da biblioteca PyYAML.

Veja abaixo alguns links úteis para saber mais sobre YAML em Python:

- [Documentação oficial da biblioteca PyYAML](https://pyyaml.org)
- [Tutorial de YAML para programadores Python](https://realpython.com/python-yaml/)
- [Exemplos avançados de uso de YAML em Python](https://github.com/yaml/pyyaml/wiki/PyYAML-yaml-examples)

## Veja também

Aqui estão algumas referências adicionais relacionadas a YAML em Python:

- [Documentação oficial do formato YAML](https://yaml.org/)
- [Artigo sobre os benefícios de usar YAML para configuração de aplicações](https://kubernetes.io/docs/concepts/configuration/overview/#why-you-should-use-yaml)
- [Apresentação sobre YAML e suas vantagens em relação a outros formatos de dados](http://assets.michaelnooren.com/yaml_conf-mini-confbucharest-dec9-2009.pdf)

Com esses recursos, esperamos que você possa aproveitar ao máximo o uso de YAML em seu código Python. Boa programação!