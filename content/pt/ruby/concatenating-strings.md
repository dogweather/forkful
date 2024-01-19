---
title:                "Concatenando strings"
html_title:           "Elixir: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# Concatenação de Strings em Ruby

## O Que e Por Quê?

Concatenar strings é o ato de juntar duas ou mais strings para formar uma única. Programadores usam esta técnica quando desejam mesclar informações separadas em uma única string, tornando a exibição de dados mais organizada e eficiente.

## Como fazer:

Aqui estão algumas maneiras de concatenar strings em Ruby. 

```Ruby
# Método 1: Pinça
nome = "Jose"
saudacao = "Olá, " + nome
puts saudacao
# Resultado será: Olá, Jose

# Método 2: Interpolação
nome = "Jose"
saudacao = "Olá, #{nome}"
puts saudacao
# Resultado será: Olá, Jose

# Método 3: Concat()
nome = "Jose"
saudacao = "Olá, ".concat(nome)
puts saudacao
# Resultado será: Olá, Jose
```

## Mergulho Profundo

Historicamente, a interpolação de strings tem sido a abordagem preferida em Ruby devido à sua eficiência e facilidade de leitura. No entanto, todos os métodos têm seus méritos.

A escolha entre "+", "concat()" ou interpolação geralmente se resume à preferência do programador ou à situação específica. "+", por exemplo, cria uma nova string, o que pode ser menos eficiente se estiver concatenando um grande número de strings. 

Em relação à implementação, "concat()" e "+" colocam a nova string no fim, enquanto a interpolação permite inseri-la em qualquer lugar.

## Veja Também

Para mais detalhes, veja:

1. [Ruby Docs: String](https://ruby-doc.org/core-2.7.3/String.html) - A documentação oficial sobre Strings em Ruby.
2. [Stack Overflow: Ruby String Concatenation](https://stackoverflow.com/questions/10076579/ruby-string-concatenation) - Discussão da comunidade sobre concatenação de strings.
3. [Sitepoint: Ruby String Interpolation](https://www.sitepoint.com/ruby-string-interpolation-tutorial/) - Tutorial detalhado sobre interpolação de strings em Ruby.