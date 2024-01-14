---
title:                "Ruby: Trabalhando com yaml"
simple_title:         "Trabalhando com yaml"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/working-with-yaml.md"
---

{{< edit_this_page >}}

## Por que trabalhar com YAML?

Se você é um programador Ruby, provavelmente já encontrou o formato de dados YAML em algum momento. Mas por que tantas vezes encontramos YAML em nossos projetos? A resposta é simples: YAML é uma forma eficiente de armazenar e transmitir dados de forma legível por humanos.

## Como usar YAML em Ruby

Como mencionado anteriormente, YAML é um formato de dados que é legível por humanos. Isso significa que é fácil de escrever e entender. Mas como podemos usá-lo em nosso código Ruby? É muito simples, basta seguir os seguintes passos:

Primeiro, precisamos requerir o módulo 'yaml' em nosso código Ruby. Isso pode ser feito usando o comando `require 'yaml'` no início do nosso arquivo.

Em seguida, podemos definir algum dados em formato de hash Ruby e convertê-los para YAML usando o método `to_yaml`. Por exemplo:

```Ruby
require 'yaml'

dados = { nome: 'João', idade: 25, cidade: 'São Paulo' }

puts dados.to_yaml
```

Isso resultará na seguinte saída:

```
--- 
nome: João 
idade: 25 
cidade: São Paulo
```

## Mergulho profundo no YAML

Além de ser uma forma legível de armazenar dados, YAML também é frequentemente usado em configurações e arquivos de manifesto. Isso porque ele suporta estruturas de dados mais complexas, como arrays e hashes aninhados. Além disso, YAML também tem a vantagem de permitir comentários, o que o torna ainda mais útil para fins de configuração.

Outra característica interessante do YAML é que ele permite referências e âncoras, o que pode ser útil para evitar a repetição de dados. Por exemplo, se quisermos definir duas variáveis com os mesmos dados, podemos usar âncoras para apontar para o mesmo valor, economizando espaço e tornando nosso código mais limpo.

Existem muitas outras funcionalidades interessantes de YAML, como a capacidade de definir aliases e seqüências personalizadas. Se você quiser aprender mais sobre como trabalhar com YAML em Ruby, recomendo explorar a documentação oficial ou alguns dos links na seção "Veja também" abaixo.

## Veja também

- [Documentação oficial do YAML](https://yaml.org/)
- [Ruby YAML - Documentação da biblioteca padrão](https://ruby-doc.org/stdlib-2.7.1/libdoc/yaml/rdoc/YAML.html)
- [Um breve guia de YAML para iniciantes](https://medium.com/swlh/yaml-for-beginners-get-started-now-f41a3a5f0535)