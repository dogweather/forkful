---
title:                "Transformando uma string em maiúsculas"
html_title:           "Ruby: Transformando uma string em maiúsculas"
simple_title:         "Transformando uma string em maiúsculas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que 

Se você está trabalhando com strings no Ruby e quer deixar as primeiras letras em maiúsculas, capitalizar é a maneira ideal de fazer isso. Isso pode ser útil em situações como mostrar nomes de pessoas ou títulos de livros.

## Como fazer

```Ruby 
"ruby".capitalize #=> "Ruby"
"olá, tudo bem?".capitalize #=> "Olá, tudo bem?"
```

Na primeira linha, a string "ruby" é capitalizada, transformando a primeira letra em maiúscula e mantendo as outras letras como minúsculas. Na segunda linha, a string "olá, tudo bem?" também é capitalizada, mas agora a primeira letra de cada palavra é maiúscula.

Outra opção é usar o método `capitalize!`, que irá alterar a string original:

```Ruby 
str = "ruby"
str.capitalize! #=> "Ruby"
str #=> "Ruby"
```

## Mergulho profundo 

O método `capitalize` é útil para casos básicos, mas pode apresentar alguns problemas caso a string já esteja em formato de título, ou seja, com as palavras já capitalizadas. Nesse caso, podemos utilizar o método `titleize`, que irá capitalizar cada palavra da string, independentemente de ela já estar capitalizada ou não.

```Ruby 
"o guia completo de Ruby".titleize #=> "O Guia Completo de Ruby"
"o guia COMPLETO de rUby".titleize #=> "O Guia Completo de Ruby"
```

Outra coisa importante a se notar é que o método `capitalize` só irá afetar a primeira letra da string, mesmo que tenha outras letras maiúsculas no meio dela. Já o método `titleize` irá formatar todas as palavras corretamente.

## Veja também

- [Documentação oficial do Ruby sobre o método `capitalize`](https://ruby-doc.org/core-#{EMOJI}String.html#method-i-capitalize)
- [Documentação oficial do Ruby sobre o método `titleize`](https://ruby-doc.org/activesupport-#{EMOJI}String.html#method-i-titleize)
- [Lista de outras manipulações de strings no Ruby](https://www.rubyguides.com/tag/strings/)