---
title:                "Ruby: Capitalizando uma string"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Alguma vez você já se deparou com um texto todo em minúsculas e pensou "Como faço para deixar todas as primeiras letras de cada palavra em maiúsculo?" Se sim, então você está no lugar certo! Neste post, vamos explorar a função de capitalizar uma string em Ruby e como ela pode ser útil em seus programas.

## Como fazer

Em Ruby, existe uma função chamada `capitalize` que permite transformar a primeira letra de uma string em maiúsculo. É importante notar que isso só afeta a primeira letra da string, todas as outras letras continuarão como estão.

Vamos ver um exemplo em ação:

```ruby
s = "ruby é incrível"
puts s.capitalize
# Saída: "Ruby é incrível"
```

Como você pode ver, a primeira palavra "ruby" agora está em letra maiúscula. Esta função pode ser bastante útil quando você precisa padronizar a capitalização em uma string em seu programa.

Mas e se quisermos capitalizar cada palavra de uma string? Para isso, existe a função `capitalize_words` que está disponível como parte da biblioteca de extensão de string.

```ruby
s = "ruby é incrível"
puts s.capitalize_words
# Saída: "Ruby É Incrível"
```

Você deve estar se perguntando por que a palavra "é" agora está com a letra "É" maiúscula. Isso porque a função `capitalize_words` segue as regras gramaticais da língua em que a string está escrita. Em português, "é" sempre é escrito com acento e em maiúsculo quando é a primeira palavra da string.

## Deep Dive

A função `capitalize` em Ruby usa o método `upcase` para transformar a primeira letra em maiúscula. O método `upcase` por si só é bastante simples, ele apenas substitui a letra minúscula por sua correspondente em maiúscula de acordo com a tabela ASCII.

Além disso, se você estiver trabalhando com strings em diferentes idiomas, pode encontrar resultados inesperados ao usar a função `capitalize`. Isso porque cada idioma possui suas próprias regras de capitalização.

## Veja também

- Documentação oficial da função `capitalize` em Ruby: https://ruby-doc.org/core-2.7.1/String.html#method-i-capitalize
- Outras funções úteis de string em Ruby: https://www.rubyguides.com/2015/05/ruby-string-methods/
- Tutorial de Ruby grátis (em português): https://tutsplus.com/pt/tutorials/tutorials/search/ruby