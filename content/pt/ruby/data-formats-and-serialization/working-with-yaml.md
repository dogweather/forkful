---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:26:36.423382-07:00
description: "Como fazer: Ruby vem com uma biblioteca integrada chamada Psych para\
  \ an\xE1lise e gera\xE7\xE3o de YAML. Para utiliz\xE1-la, voc\xEA primeiro precisa\
  \ requerer a\u2026"
lastmod: '2024-03-13T22:44:47.115553-06:00'
model: gpt-4-0125-preview
summary: "Ruby vem com uma biblioteca integrada chamada Psych para an\xE1lise e gera\xE7\
  \xE3o de YAML."
title: Trabalhando com YAML
weight: 41
---

## Como fazer:
Ruby vem com uma biblioteca integrada chamada Psych para análise e geração de YAML. Para utilizá-la, você primeiro precisa requerer a biblioteca padrão YAML. Aqui está um exemplo básico para começar:

```ruby
require 'yaml'

# Hash a ser serializado
person = { name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"] }

# Convertendo o hash para YAML
yaml_data = person.to_yaml

puts yaml_data
```

**Saída de Exemplo:**

```yaml
---
:name: John Doe
:age: 30
:skills:
- Ruby
- JavaScript
```

Para carregar dados YAML de volta em um objeto Ruby:

```ruby
loaded_person = YAML.load(yaml_data)

puts loaded_person
```

**Saída de Exemplo:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

### Usando Bibliotecas de Terceiros:
Embora a biblioteca padrão seja suficiente para tarefas básicas, para necessidades complexas você pode procurar gems de terceiros como 'safe_yaml'. Para usar essas bibliotecas, você deve primeiro instalar a gem:

```bash
gem install safe_yaml
```

Depois, você pode usá-la para carregar dados YAML de forma segura, mitigando riscos como instanciação de objetos a partir de fontes controladas pelo usuário:

```ruby
require 'safe_yaml'

safe_loaded_person = SafeYAML.load(yaml_data)

puts safe_loaded_person
```

**Saída de Exemplo:**

```ruby
{name: "John Doe", age: 30, skills: ["Ruby", "JavaScript"]}
```

Essa abordagem aprimora a segurança do seu manuseio de YAML, tornando-a uma boa escolha para aplicações que carregam YAML de fontes não confiáveis.
