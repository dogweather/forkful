---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Ruby: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que
Você pode querer excluir caracteres que correspondam a um determinado padrão em uma string por várias razões, como limpar dados, filtrar informações específicas ou modificar a formatação de um texto.

## Como Fazer
Para excluir caracteres que correspondam a um padrão em uma string, podemos utilizar o método `gsub` do Ruby. Ele recebe dois argumentos: o padrão a ser procurado e o que deve ser substituído no lugar do padrão.

Exemplo:

````Ruby
# definindo uma string com diversos caracteres especiais
string = "Hello! This is a #sample# string!"
# utilizando o método gsub para substituir os caracteres '#' por vazio
string.gsub("#", "")
# output => "Hello! This is a sample string!"
````

Caso queiramos substituir mais de um carácter ou utilizar uma expressão regular como padrão, podemos passar um bloco de código para o método `gsub`. Assim, a correspondência do padrão será capturada pelo bloco e podemos manipular essa informação antes de substituí-la.

Exemplo:

````Ruby
# definindo uma string com diversos caracteres especiais
string = "Dia 20/04/2021 foi incrível!"
# utilizando gsub com uma expressão regular para substituir a data no formato original pelo formato dia-mês-ano
string.gsub(/(\d{2})\/(\d{2})\/(\d{4})/, '\1-\2-\3')
# output => "Dia 20-04-2021 foi incrível!"
````

## Mergulho Profundo
É importante lembrar que o método `gsub` não modifica a string original, mas sim retorna uma nova string com as alterações realizadas. Além disso, também é possível utilizar outros métodos de manipulação de strings, como o `delete`, para realizar a exclusão de caracteres em uma string.

Para saber mais sobre os métodos de manipulação de strings do Ruby, acesse [a documentação oficial](https://ruby-doc.org/core-3.0.0/String.html) ou confira o [artigo da DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-string-methods-in-ruby) sobre o assunto.

## Veja Também
- [Documentação oficial do método `gsub`](https://ruby-doc.org/core-3.0.0/String.html#method-i-gsub)
- [Artigo da DigitalOcean sobre métodos de manipulação de strings em Ruby](https://www.digitalocean.com/community/tutorials/how-to-use-string-methods-in-ruby)