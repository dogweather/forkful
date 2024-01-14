---
title:                "Ruby: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que usar expressões regulares?

As expressões regulares são uma poderosa ferramenta para trabalhar com strings em Ruby. Com elas, é possível realizar buscas, extrações e manipulações de texto de forma eficiente e precisa. Se você trabalha com programação, provavelmente já se deparou com a necessidade de lidar com padrões em dados de texto. As expressões regulares simplificam esse processo, permitindo que você crie regras para encontrar e manipular esses padrões de forma rápida e efetiva.

## Como utilizar expressões regulares em Ruby?

Para utilizar as expressões regulares em Ruby, podemos utilizar a classe `Regexp` que já vem embutida na linguagem. Ela possui diversos métodos que nos permitem criar e manipular as expressões. Veja um exemplo de como podemos utilizar uma expressão regular para buscar um padrão específico em uma string:

```Ruby
string = "O nome do nosso planeta é Terra."
pattern = /planeta/
if string =~ pattern
  puts "Encontrei o padrão!"
end

# Saída: Encontrei o padrão!
```

Neste exemplo, criamos uma expressão regular para buscar a palavra "planeta" em uma string e utilizamos o operador `=~` para verificar se a expressão foi encontrada. Existem diversos outros métodos e operadores que podem ser utilizados com expressões regulares em Ruby, então é importante estudar a documentação para aprofundar seus conhecimentos.

## Aprofundando-se nas expressões regulares

As expressões regulares possuem uma sintaxe própria e, para utilizá-las efetivamente, é importante entender como ela funciona. Um bom ponto de partida é entender quais são os caracteres especiais utilizados em expressões regulares e como eles podem ser combinados para criar padrões mais complexos. Além disso, existem algumas técnicas úteis que podem ser empregadas, como o uso de grupos de captura e o uso de operadores como `*` (zero ou mais ocorrências) e `+` (uma ou mais ocorrências).

Para se aprofundar ainda mais no assunto, recomendo a leitura do livro "Introdução às Expressões Regulares" de O'Reilly Media, que aborda o tema de forma abrangente e prática, com exemplos em várias linguagens, incluindo Ruby.

## Veja também

- [Documentação oficial do Ruby sobre expressões regulares](https://ruby-doc.org/core/Regexp.html)
- ["Introdução às Expressões Regulares" (em inglês)](https://www.oreilly.com/library/view/introducing-regular-expressions/9781449338883/)