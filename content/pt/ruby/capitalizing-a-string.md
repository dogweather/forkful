---
title:    "Ruby: Maiúsculas em uma string"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Por que Capitalizar uma String?

Capitalizar uma string é uma tarefa comum em programação Ruby. Isso significa transformar a primeira letra de cada palavra em maiúscula. Isso é útil em diversos casos, como na formatação de nomes, títulos de livros ou em outras aplicações de texto. Neste artigo, vamos desvendar como fazer isso em Ruby de forma simples e eficiente.

## Como Fazer

Para capitalizar uma string, usamos o método `.capitalize`, que está embutido na classe `String` do Ruby. Sua sintaxe é a seguinte:

```Ruby
string.capitalize
```

Por exemplo, se temos a string "ruby é uma linguagem de programação incrível", podemos capitalizar ela da seguinte forma:

```Ruby
"ruby é uma linguagem de programação incrível".capitalize
# Saída: "Ruby é uma linguagem de programação incrível"
```

Podemos também capitalizar apenas a primeira letra de uma string, deixando as demais minúsculas, utilizando o método `.capitalize!` com o sinal de exclamação no final:

```Ruby
"linguagem".capitalize!
# Saída: "Linguagem"
```

Se quisermos capitalizar todas as letras da string, podemos utilizar o método `.upcase`:

```Ruby
"ruby".upcase
# Saída: "RUBY"
```

E para transformar todas as letras em minúsculas, usamos o método `.downcase`:

```Ruby
"RUBY".downcase
# Saída: "ruby"
```

## Uma Olhada Mais Profunda

Ao usar o método `.capitalize`, percebemos que ele não capitaliza apenas a primeira letra de cada palavra. Ele também transforma todas as letras que forem maiúsculas em minúsculas. Isso acontece porque o método é baseado no algoritmo de capitalização do Unix, que transforma todas as letras em minúsculas e em seguida, apenas a primeira letra da string em maiúscula.

No entanto, se quisermos capitalizar apenas a primeira letra, preservando as demais como estão, podemos utilizar o método `.replace` e passar uma substring como argumento para ele:

```Ruby
string = "ruby é uma linguagem de programação incrível"
string.replace(string[0].upcase + string[1..-1])
# Saída: "Ruby é uma linguagem de programação incrível"
```

## Veja Também

- Documentação do método `.capitalize` (em inglês): https://ruby-doc.org/core-2.6/String.html#method-i-capitalize
- Outros métodos de formatação de string em Ruby (em inglês): https://www.rubyguides.com/2019/10/ruby-string-methods/