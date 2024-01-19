---
title:                "Capitalizando uma string"
html_title:           "Ruby: Capitalizando uma string"
simple_title:         "Capitalizando uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Capitalizar uma string significa transformar a primeira letra de cada palavra numa letra maiúscula. Programadores fazem isso para melhorar a legibilidade e a aparência dos dados exibidos.

## Como fazer?

Vamos usar o método `capitalize` disponível em Ruby para capitalizar uma string. Ele retorna uma nova string com a primeira letra em maiúscula e o restante em minúsculas.

```Ruby
s = "olá mundo!"
s.capitalize
=> "Olá mundo!"

s = "vamos programar em ruby"
s.split.map(&:capitalize).join(' ')
=> "Vamos Programar Em Ruby"
```

## Uma análise aprofundada

Por muitos anos, a capitalização foi usada em linguagens de programação e continua sendo uma prática comum por ser uma ótima forma de melhorar a legibilidade. 
Existem alguns métodos alternativos além do `capitalize` em Ruby. O método `titleize` do Rails faz algo similar, porém, ele insere uma letra maiúscula a começar de cada palavra e minúscula no restante, perfeito para títulos. 

A implementação do metodo `capitalize` em Ruby é bastante simples. Ele basicamente percorre a string e transfere cada letra para maiúscula ou minúscula dependendo da sua posição.

```Ruby
class String
  def capitalize
    self[0].upcase + self[1..-1].downcase
  end
end
```
É importante lembrar que `capitalize` não modifica a string original, mas retorna uma nova string.

## Veja também

[A identidade Ruby: Guia de estilo de codificação Ruby](https://github.com/rubocop/ruby-style-guide#naming)

[Documentação Ruby](https://ruby-doc.org/core-2.7.2/String.html)

[Documentação Rails - titleize](https://api.rubyonrails.org/classes/String.html#method-i-titleize)