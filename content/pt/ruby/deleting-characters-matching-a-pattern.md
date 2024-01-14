---
title:                "Ruby: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Por que excluir caracteres que correspondem a um padrão?

Às vezes, ao trabalhar com strings em um programa Ruby, pode ser necessário excluir certos caracteres que correspondem a um padrão específico. Isso pode ser útil para manipular ou filtrar dados de forma mais eficiente. Neste post, vamos explorar como fazer isso usando Ruby.

## Como fazer:

Para excluir caracteres correspondentes a um padrão em uma string em Ruby, podemos usar o método gsub (substituir globalmente). Este método permite que substituamos uma determinada sequência de caracteres de uma string por outra. No entanto, com o uso do modificador "!", podemos usá-lo para excluir caracteres em vez de substituí-los.

Aqui está um exemplo de código que usa gsub para excluir todos os caracteres não numéricos de uma string:

```Ruby
nome = "João123Lima!"
nome.gsub!(/[^0-9]/, "")
```

Neste exemplo, usamos a expressão regular /[^0-9]/ para corresponder a todos os caracteres que não são números. O modificador "!" no final do método gsub garante que essa substituição seja feita diretamente na string original, em vez de retornar uma nova string. Portanto, a variável "nome" agora seria "123".

## Profundidade:

Além do modificador "!", a referência do método gsub em Ruby oferece outros modificadores que podem ser úteis para excluir caracteres correspondentes a um padrão. Por exemplo, se quisermos ignorar maiúsculas e minúsculas ao procurar correspondências, podemos usar o modificador "i". Se quisermos limitar o número de substituições a serem feitas, também podemos usar o modificador "n".

Outra opção é usar o método delete, que é semelhante ao gsub, mas apenas remove os caracteres correspondentes em vez de substituí-los por outros. Aqui está um exemplo de código que usa delete para excluir todos os caracteres que não são letras:

```Ruby
frase = "Eu tenho 123 maçãs!"
frase.delete!("^a-z")
```

Após a execução deste código, a variável "frase" seria "Eutnhmaçãs".

## Veja também:

- [Documentação do método gsub em Ruby](https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub)
- [Documentação do método delete em Ruby](https://ruby-doc.org/core-2.6.3/String.html#method-i-delete)
- [Tutorial de expressões regulares em Ruby](https://www.tutorialspoint.com/ruby/ruby_regular_expressions.htm)

É isso aí, esperamos que este post seja útil para aprender a excluir caracteres que correspondem a um padrão em Ruby! Experimente esses métodos em seus próprios projetos e compartilhe suas experiências nos comentários abaixo.