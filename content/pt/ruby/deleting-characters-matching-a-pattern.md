---
title:                "Excluindo caracteres correspondentes a um padrão"
html_title:           "Ruby: Excluindo caracteres correspondentes a um padrão"
simple_title:         "Excluindo caracteres correspondentes a um padrão"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Deletar caracteres que correspondem a um padrão é um processo importante na programação em Ruby. Isso permite que os programadores removam caracteres específicos de uma string ou documento, economizando tempo e simplificando o código. É uma técnica útil para limpar dados ou formatar strings de maneira consistente.

## Como Fazer:

Para deletar caracteres que correspondem a um padrão, podemos usar o método `gsub` em uma string. Esse método substitui todos os caracteres que correspondem ao padrão com uma string vazia, ou seja, os deleta. Veja um exemplo abaixo:

```Ruby
test_string = "Olá Ruby!"
new_string = test_string.gsub("a", "")
puts new_string # Output: Ol Ruby!
```

Neste exemplo, usamos o método `gsub` para deletar todas as letras "a" de uma string. Isso pode ser útil em situações onde queremos remover caracteres desejados de uma string.

## Mergulho Profundo:

O método `gsub` é muito útil para deletar caracteres em uma string, mas é importante ter em mente que ele irá deletar todas as ocorrências do padrão. Se você deseja apenas deletar a primeira ocorrência de cada padrão, pode usar o método `sub`. Além disso, também é possível usar expressões regulares com o método `gsub`. Isso permite que você especifique padrões mais complexos para serem deletados. Por exemplo:

```Ruby
test_string = "ruby123!%"
new_string = test_string.gsub(/[^a-zA-Z]/, "")
puts new_string # Output: ruby!
```

Neste exemplo, usamos uma expressão regular para selecionar apenas letras minúsculas e maiúsculas e deletar todos os outros caracteres.

## Veja Também:

Para mais informações sobre o método `gsub` e expressões regulares em Ruby, confira a [documentação oficial da linguagem](https://ruby-doc.org/core-2.7.0/src/doc/syntax/literals_rdoc.html#label-Basic+Patterns). Também é possível encontrar mais exemplos e dicas em fóruns de desenvolvedores e comunidades de Ruby. Experimente diferentes padrões e veja como eles afetam a saída para se familiarizar com essa técnica útil de manipulação de strings.