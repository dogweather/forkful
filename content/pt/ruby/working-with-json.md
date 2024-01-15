---
title:                "Trabalhando com json"
html_title:           "Ruby: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?

JSON (JavaScript Object Notation) é um formato extremamente popular para troca de dados entre diferentes sistemas. Ele é amplamente utilizado em aplicações web e mobile, e é suportado por diversas linguagens de programação, incluindo Ruby. Trabalhar com JSON pode tornar a comunicação entre diferentes sistemas mais eficiente e fácil de entender.

## Como fazer:

Vamos ver alguns exemplos simples de como trabalhar com JSON em Ruby:

```Ruby
# Convertendo um objeto Ruby em JSON
require 'json'

# Criando um objeto Ruby
person = {
  name: "João",
  age: 30,
  occupation: "programador"
}

# Convertendo o objeto para JSON
json_person = person.to_json
puts json_person
# => {"name":"João","age":30,"occupation":"programador"}

```

```Ruby
# Convertendo um JSON em objeto Ruby
json_car = '{"brand":"Toyota","model":"Corolla","color":"preto","year":2020}'

# Convertendo o JSON para objeto Ruby
car = JSON.parse(json_car)
puts car
# => { "brand" => "Toyota", "model" => "Corolla","color" => "preto", "year" => 2020 }
```

```Ruby
# Trabalhando com arquivos JSON
require 'json'

# Lendo um arquivo JSON
json_data = File.read('./data.json')

# Convertendo o JSON para objeto Ruby
data = JSON.parse(json_data)
puts data
# => { "name" => "Maria", "age" => 25, "occupation" => "designer" }
```

## Aprofundando-se:

Além das funções básicas de conversão entre objetos Ruby e JSON, existem algumas outras funcionalidades interessantes quando se trabalha com JSON em Ruby:

- É possível adicionar opções ao método `to_json` para controlar o formato da saída, como por exemplo a indentação.
- Ao converter um objeto Ruby em JSON, é possível incluir apenas os atributos desejados utilizando o método `slice`.
- Também é possível trabalhar com arrays e hashes aninhados em JSON, tornando a estrutura de dados mais complexa.

Para mais informações e detalhes sobre como trabalhar com JSON em Ruby, consulte a documentação oficial da linguagem ou outros recursos online.

## Veja também:

- [Documentação oficial do Ruby sobre JSON](https://ruby-doc.org/stdlib-2.7.0/libdoc/json/rdoc/JSON.html)
- [Guia de referência de JSON em Ruby](https://www.rubyguides.com/2018/10/ruby-json-library/)
- [Exemplos práticos de como trabalhar com JSON em Ruby](https://www.rubyguides.com/2015/09/ruby-json-what-you-need-to-know/)