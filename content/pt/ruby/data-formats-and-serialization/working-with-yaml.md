---
title:                "Trabalhando com YAML"
aliases:
- /pt/ruby/working-with-yaml.md
date:                  2024-02-03T19:26:36.423382-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com YAML"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-yaml.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O que & Por quê?
YAML, que significa YAML Ain't Markup Language (YAML Não é Uma Linguagem de Marcação), é amplamente utilizado em Ruby para arquivos de configuração e serialização de dados devido ao seu formato legível por humanos. Os programadores gravitam em torno do YAML quando precisam armazenar ou transmitir objetos de dados de maneira legível, porém estruturada, simplificando tarefas como gerenciamento de configuração, armazenamento de dados e compartilhamento de dados entre linguagens.

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
