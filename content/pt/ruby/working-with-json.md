---
title:                "Ruby: Trabalhando com json"
simple_title:         "Trabalhando com json"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-json.md"
---

{{< edit_this_page >}}

## Por que trabalhar com JSON?

JSON, ou JavaScript Object Notation, é um formato popular para armazenar e transmitir dados estruturados. Ele é leve, fácil de ler e escrever, e é suportado por muitas linguagens de programação, incluindo Ruby. Trabalhar com JSON é importante para desenvolvedores que desejam criar aplicativos e serviços da web modernos e integrados.

## Como trabalhar com JSON em Ruby

Para trabalhar com JSON em Ruby, primeiro é necessário carregar a biblioteca padrão "json". Em seguida, é possível converter um objeto Ruby, como um hash ou array, em uma string JSON usando o método `to_json`. Da mesma forma, é possível converter uma string JSON em um objeto Ruby usando o método `JSON.parse`.

Um exemplo de como converter um hash em uma string JSON:

```Ruby
require 'json'

hash = { name: "João", age: 25}
puts hash.to_json
```

Resultado:

```
{"name":"João","age":25}
```

Ou converter uma string JSON em um objeto Ruby:

```Ruby
require 'json'

json = '{"name":"Maria","age":30}'
puts JSON.parse(json)
```

Resultado:

```
{ "name"=>"Maria", "age"=>30 }
```

Também é possível utilizar blocos de código `do..end` para personalizar a conversão de objetos mais complexos para JSON. Por exemplo:

```Ruby
require 'json'

class Person
  attr_accessor :name, :age

  def initialize(name, age)
    @name = name
    @age = age
  end

  # Converte a instância em um hash para ser convertido em JSON
  def as_json
    { name: @name, age: @age }
  end
end

person = Person.new("Ana", 28)
puts person.to_json
```

Resultado:

```
{"name":"Ana","age":28}
```

## Profundando em JSON

Além de converter objetos Ruby em JSON e vice-versa, é possível trabalhar com arquivos JSON utilizando a classe `File`. Por exemplo, para ler um arquivo JSON e converter seus dados em um objeto Ruby:

```Ruby
require 'json'

json_file = File.read("data.json")
data = JSON.parse(json_file)
```

Para escrever em um arquivo JSON, primeiro é necessário converter um objeto Ruby em JSON e depois escrevê-lo no arquivo:

```Ruby
require 'json'

person = { name: "José", age: 35 }
json = person.to_json

File.write("person.json", json)
```

## Veja também

- [Documentação do Ruby sobre JSON](https://ruby-doc.org/stdlib-2.6.3/libdoc/json/rdoc/JSON.html)
- [Tutorial em português sobre JSON em Ruby](https://www.ti-enxame.com/pt/ruby/json-ruby-guia-do-iniciante/941916051/)
- [Referência completa da linguagem JSON](https://www.json.org/json-pt.html)