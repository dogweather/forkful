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

## O que & Por quê?

Trabalhar com JSON é uma habilidade importante para os programadores, pois JSON é uma formatação comum e eficiente para troca de dados entre sistemas. Ele é amplamente usado em aplicações web e mobile, tornando-se um conhecimento necessário para quem deseja desenvolver nessas áreas.

## Como fazer:

```Ruby
# Convertendo um objeto Ruby para JSON
require 'json'

objeto_ruby = {
  "nome": "João",
  "idade": 25
}

json = objeto_ruby.to_json
puts json
```
Output: `{"nome":"João","idade":25}`

```Ruby
# Convertendo JSON para um objeto Ruby
require 'json'

json = '{"nome":"Maria","idade":30}'
objeto_ruby = JSON.parse(json)

puts objeto_ruby["nome"]
puts objeto_ruby["idade"]
```
Output: `Maria` e `30`

## Mergulho Profundo:

JSON (JavaScript Object Notation) é uma formatação de dados baseada em texto que se tornou popular nos últimos anos. Ela é derivada da linguagem JavaScript, mas pode ser usada com qualquer linguagem de programação. Algumas alternativas para JSON incluem XML e YAML. No entanto, JSON é geralmente preferido devido à sua simplicidade e tamanho reduzido. Para trabalhar com JSON em Ruby, é necessário o uso da biblioteca padrão `json` e é possível convertê-lo facilmente para objetos Ruby usando `JSON.parse()` e objetos Ruby para JSON usando `to_json()`.

## Veja também:

- [Documentação da biblioteca JSON em Ruby](https://ruby-doc.org/stdlib-2.6.3/libdoc/json/rdoc/JSON.html)
- [Introdução ao JSON](https://www.json.org/)