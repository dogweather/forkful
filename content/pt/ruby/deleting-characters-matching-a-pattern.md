---
title:                "Excluindo caracteres que correspondem a um padrão"
html_title:           "Arduino: Excluindo caracteres que correspondem a um padrão"
simple_title:         "Excluindo caracteres que correspondem a um padrão"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Deletando Caracteres Correspondentes a um Padrão em Ruby

## O quê & Por quê?
Deletar caracteres que correspondem a um padrão em uma string é uma tarefa comum em programação. Os programadores fazem isso para limpar dados, preparar entrada de texto para o processamento ou remover caracteres indesejados.

## Como Fazer:
Em Ruby, podemos usar o método `delete` para excluir caracteres de uma string que correspondem a um padrão específico. Em seguida, temos um exemplo simples demonstrando isso.

```Ruby
str = "Olá, mundo! 123"
str_limpo = str.delete "!, 123"
puts str_limpo
```
Nesse caso, a saída seria:

```Ruby
Olámundo
```

## Mergulho Profundo

O método `delete` foi introduzido no Ruby 1.6.0 e tem sido mantido desde então devido à sua utilidade. Existem outras alternativas como `gsub` e `tr`, mas `delete` é mais direto quando se trata de remover caracteres que correspondem a um padrão.

Vamos olhar um exemplo usando `tr`

```Ruby
str = "Olá, mundo! 123"
str_limpo = str.tr("!, 123", "")
puts str_limpo
```

Com `tr`, especificamos dois argumentos onde o segundo é sempre vazio se quisermos deletar caracteres. `delete` é um pouco mais intuitivo neste caso.

O método `delete` na verdade cria uma nova string sem os caracteres especificados. Ele não modifica a string original, então se você precisa da string original inalterada, `delete` é a melhor escolha.

## Veja Também:

- Documentação Ruby sobre `String#delete`: [https://ruby-doc.org/core-2.7.3/String.html#method-i-delete](https://ruby-doc.org/core-2.7.3/String.html#method-i-delete)
- 'How to Remove a Character from String in Ruby' na Stack Overflow: [https://stackoverflow.com/questions/8437993/how-to-remove-a-character-from-string-in-ruby](https://stackoverflow.com/questions/8437993/how-to-remove-a-character-from-string-in-ruby)
- 'Ruby String delete Method': [https://www.tutorialspoint.com/ruby/ruby_string_delete.htm](https://www.tutorialspoint.com/ruby/ruby_string_delete.htm)