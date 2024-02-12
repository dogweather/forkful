---
title:                "Trabalhando com JSON"
aliases: - /pt/ruby/working-with-json.md
date:                  2024-02-03T19:23:58.426621-07:00
model:                 gpt-4-0125-preview
simple_title:         "Trabalhando com JSON"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que e Por Quê?

JSON (JavaScript Object Notation) é um formato leve de troca de dados, prevalente em aplicações web para a troca de dados entre clientes e servidores. Programadores trabalham com JSON em Ruby para analisar dados recebidos de fontes externas ou para formatar dados para respostas de API, aproveitando sua estrutura legível por humanos para fácil manipulação e armazenamento de dados.

## Como:

Ruby, com sua biblioteca padrão, oferece maneiras simples de analisar e gerar JSON. O módulo principal para essas operações é `json`, que pode ser facilmente integrado em qualquer aplicação Ruby.

### Analisando JSON:

Para converter uma string JSON em um hash Ruby, você pode usar o método `JSON.parse`.

```ruby
require 'json'

json_string = '{"name": "John Doe", "age": 30, "city": "New York"}'
ruby_hash = JSON.parse(json_string)

puts ruby_hash
# Saída: {"name"=>"John Doe", "age"=>30, "city"=>"New York"}
```

### Gerando JSON:

Da mesma forma, para converter um hash Ruby em uma string JSON, usa-se o método `JSON.generate` ou o método `to_json` disponível nos objetos Ruby uma vez que a biblioteca `json` é requerida.

```ruby
require 'json'

ruby_hash = { name: "Jane Doe", age: 25, city: "Los Angeles" }
json_string = ruby_hash.to_json

puts json_string
# Saída: {"name":"Jane Doe","age":25,"city":"Los Angeles"}
```

### Bibliotecas de Terceiros:

Embora a biblioteca padrão do Ruby cubra o manuseio básico de JSON, muitos projetos dependem de bibliotecas de terceiros para funcionalidade aprimorada e desempenho. Uma escolha popular é `Oj` (Optimized JSON).

#### Analisando com Oj:

```ruby
require 'oj'

json_string = '{"name": "Alex", "age": 40, "city": "Chicago"}'
ruby_hash = Oj.load(json_string)

puts ruby_hash
# Saída: {"name"=>"Alex", "age"=>40, "city"=>"Chicago"}
```

#### Gerando com Oj:

Oj também oferece uma maneira rápida de gerar JSON a partir de objetos Ruby:

```ruby
require 'oj'

ruby_hash = { name: "Samantha", age: 35, city: "Miami" }
json_string = Oj.dump(ruby_hash)

puts json_string
# Saída: {"name":"Samantha","age":35,"city":"Miami"}
```

Esses exemplos ilustram a natureza direta de trabalhar com JSON em Ruby, tornando-o acessível para tarefas que vão desde manipulações simples de dados até comunicações complexas de API.
