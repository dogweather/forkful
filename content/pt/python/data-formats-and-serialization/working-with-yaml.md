---
aliases:
- /pt/python/working-with-yaml/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:30.378936-07:00
description: "YAML, que significa \"YAML Ain't Markup Language\" (YAML N\xE3o \xE9\
  \ Uma Linguagem de Marca\xE7\xE3o), \xE9 um formato de serializa\xE7\xE3o de dados\
  \ leg\xEDvel por humanos.\u2026"
lastmod: 2024-02-18 23:08:57.781097
model: gpt-4-0125-preview
summary: "YAML, que significa \"YAML Ain't Markup Language\" (YAML N\xE3o \xE9 Uma\
  \ Linguagem de Marca\xE7\xE3o), \xE9 um formato de serializa\xE7\xE3o de dados leg\xED\
  vel por humanos.\u2026"
title: Trabalhando com YAML
---

{{< edit_this_page >}}

## O que & Por quê?
YAML, que significa "YAML Ain't Markup Language" (YAML Não é Uma Linguagem de Marcação), é um formato de serialização de dados legível por humanos. Programadores usam YAML para arquivos de configuração, mensagens entre processos e armazenamento de dados devido à sua sintaxe simples e fácil legibilidade em comparação com outros formatos como XML ou JSON.

## Como fazer:
Ler e escrever em YAML com Python normalmente envolve o uso de uma biblioteca de terceiros, sendo a `PyYAML` a mais popular. Para começar, você precisará instalar o PyYAML executando `pip install PyYAML`.

**Exemplo: Escrevendo em um Arquivo YAML**

```python
import yaml

data = {'uma lista': [1, 42, 3.141, 1337, 'ajuda', u'€'],
        'uma string': 'boo!',
        'outro dicionário': {'foo': 'bar', 'chave': 'valor', 'a resposta': 42}}

with open('exemplo.yaml', 'w') as f:
    yaml.dump(data, f, default_flow_style=False)

# Isso cria `exemplo.yaml` com os dados estruturados no formato YAML.
```

**Exemplo: Lendo de um Arquivo YAML**

```python
import yaml

with open('exemplo.yaml', 'r') as f:
    dados_carregados = yaml.safe_load(f)

print(dados_carregados)

# Saída: 
# {'uma lista': [1, 42, 3.141, 1337, 'ajuda', '€'],
#  'uma string': 'boo!',
#  'outro dicionário': {'foo': 'bar', 'chave': 'valor', 'a resposta': 42}}
```

**Usando YAML para Configuração**

Muitos programadores usam YAML para gerenciar configurações de aplicativos. Aqui está um exemplo de como alguém pode estruturar um arquivo de configuração e lê-lo:

config.yaml:
```yaml
database:
  host: localhost
  port: 5432
  username: admin
  password: secreto
```

Lendo o arquivo de configuração em Python:
```python
import yaml

with open('config.yaml', 'r') as f:
    config = yaml.safe_load(f)

print(config['database']['host'])  # Saída: localhost
```

**Tratando Estruturas Complexas**

Para estruturas complexas, PyYAML permite que você defina objetos Python personalizados. No entanto, assegure práticas seguras usando `safe_load` para evitar a execução de funções ou objetos arbitrários.

```python
import yaml

# Definir um objeto Python
class Exemplo:
    def __init__(self, valor):
        self.valor = valor

# Construtor personalizado
def construtor_exemplo(loader, node):
    valor = loader.construct_scalar(node)
    return Exemplo(valor)

# Adicionar construtor para a tag "!example"
yaml.add_constructor('!example', construtor_exemplo)

yaml_str = "!example 'dados'"
carregado = yaml.load(yaml_str, Loader=yaml.FullLoader)

print(carregado.valor)  # Saída: dados
```

Neste trecho, `!example` é uma tag personalizada usada para instanciar um objeto `Exemplo` com o valor 'dados' de uma string YAML. Carregadores personalizados como este expandem a flexibilidade do PyYAML, permitindo o processamento de estruturas e tipos de dados mais complexos.
