---
title:                "Ruby: Capitalizando uma String"
simple_title:         "Capitalizando uma String"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que Capitalizar um String em Ruby?

Uma questão comum que muitos programadores iniciantes em Ruby têm é por que é importante capitalizar um string. A resposta é simples: capitalizar um string é uma maneira de melhorar a legibilidade e organização do seu código. Além disso, em muitos casos, é necessário capitalizar strings para que eles correspondam a determinadas condições ou critérios em um programa.

## Como Fazer a Capitalização de um String em Ruby

Para capitalizar um string em Ruby, podemos usar o método `capitalize` ou `capitalize!`. Veja um exemplo abaixo:

````Ruby
string = "bem vindo"
puts string.capitalize
````

O output será:

````Ruby
Bem vindo
````

Podemos ver que o primeiro caractere do string foi convertido para letra maiúscula.

Também podemos usar o método `upcase` ou `upcase!` para capitalizar todas as letras do string. Veja um exemplo:

````Ruby
string = "bem vindo"
puts string.upcase
````

O output será:

````Ruby
BEM VINDO
````

## Mergulho Profundo: Mais Informações Sobre a Capitalização de Strings em Ruby

Além dos métodos mencionados acima, Ruby também tem o método `capitalize!` que muda permanentemente o valor do string para uma versão capitalizada. Isso significa que, se você chamar esse método no string original, ele será alterado permanentemente em vez de apenas ser exibido no output.

Também é importante lembrar que a capitalização de strings em Ruby respeita a formatação dos caracteres do alfabeto. Por exemplo, em português, a letra `ç` só é capitalizada se estiver no início da frase, caso contrário, ela permanecerá minúscula.

## Veja Também

- [Método `capitalize` na documentação oficial do Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize)
- [Método `upcase` na documentação oficial do Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-upcase)
- [Método `capitalize!` na documentação oficial do Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize-21)