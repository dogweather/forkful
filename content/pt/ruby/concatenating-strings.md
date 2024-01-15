---
title:                "Unindo strings"
html_title:           "Ruby: Unindo strings"
simple_title:         "Unindo strings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

## Porque

Se você está trabalhando com Ruby, é provável que em algum momento precise unir strings. A concatenação de strings é uma tarefa comum na programação e é útil para juntar várias partes de um texto em uma única string.

## Como Fazer

Para unir strings em Ruby, você pode utilizar o operador `+` ou o método `concat`.

```ruby
string_1 = "Olá"
string_2 = "mundo!"

puts string_1 + " " + string_2
# saída: Olá mundo!

string_1 = "Ruby"

puts string_1.concat(" é", " uma", " linguagem", " de", " programação")
# saída: Ruby é uma linguagem de programação
```

Você também pode utilizar o método `<<` para concatenar strings.

```ruby
string_1 = "Eu amo "
string_2 = "Ruby!"

string_1 << string_2

puts string_1
# saída: Eu amo Ruby!
```

Além disso, é possível utilizar o método `join` em um array de strings para unir todos os elementos em uma única string.

```ruby
array = ["Hoje", "é", "um", "novo", "dia"]

puts array.join(" ")
# saída: Hoje é um novo dia
```

## Mergulho Profundo

Ao utilizar o operador `+` para concatenar strings, o Ruby cria uma nova string no processo, o que pode afetar o desempenho do seu código. Já os métodos `concat` e `<<` modificam a string original, o que pode ser mais eficiente em termos de tempo de execução.

Além disso, é importante notar que os métodos `concat` e `<<` podem receber mais de um argumento, enquanto o operador `+` só pode ser utilizado entre duas strings. Isso significa que ao concatenar mais de duas strings, você pode economizar tempo e linhas de código usando um desses métodos.

## Veja também

- [Documentação oficial do Ruby sobre concatenação de strings](https://ruby-doc.org/core-2.7.2/String.html#method-i-2B)
- [Artigo da Computer Science for Fun sobre concatenação de strings em Ruby](https://www.comp.nus.edu.sg/~stevenha/visualization/parenthesis.html)