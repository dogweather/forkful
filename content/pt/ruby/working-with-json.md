---
title:                "Trabalhando com JSON"
date:                  2024-01-19
html_title:           "Arduino: Trabalhando com JSON"
simple_title:         "Trabalhando com JSON"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Trabalhar com JSON significa manipular um formato leve de troca de dados fácil de ler e escrever por humanos, e simples de ser analisado e gerado por máquinas. Programadores utilizam JSON porque é um padrão universal em APIs de web e é útil para armazenar e transportar dados estruturados.

## Como Fazer:

```Ruby
require 'json'

# Criando um hash e convertendo para string JSON
meus_dados = { nome: "João", idade: 30, cidade: "Lisboa" }
json_string = meus_dados.to_json
puts json_string
# => {"nome":"João","idade":30,"cidade":"Lisboa"}

# Analisando uma string JSON e convertendo para um hash Ruby
json_recebido = '{"nome":"Ana","idade":25,"cidade":"Porto"}'
dados_ruby = JSON.parse(json_recebido)
puts dados_ruby
# => {"nome"=>"Ana", "idade"=>25, "cidade"=>"Porto"}

# Tratamento de exceções ao analisar JSON inválido
begin
  invalid_json = '{"nome": "Miguel", "idade": trinta}'
  JSON.parse(invalid_json)
rescue JSON::ParserError => e
  puts "Ocorreu um erro ao analisar o JSON: #{e.message}"
end
```

## Aprofundamento

JSON, sigla para JavaScript Object Notation, foi originalmente derivado da notação de objeto do JavaScript, mas agora é um padrão independente de linguagem com códigos e bibliotecas disponíveis para muitas linguagens, incluindo Ruby. Alternativas ao JSON incluem XML e YAML, mas JSON é geralmente preferido por sua facilidade de uso e velocidade. Ruby incorpora métodos de análise e geração de JSON na classe principal `JSON`, e a implementação é direta: convertendo objetos Ruby para strings JSON e vice-versa.

## Veja Também:

- Guia rápido de JSON: [http://json.org/](http://json.org/)
