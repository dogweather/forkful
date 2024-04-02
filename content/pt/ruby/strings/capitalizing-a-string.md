---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Capitalizar uma string geralmente significa converter o primeiro caractere\
  \ de uma string para mai\xFAscula e o restante para min\xFAscula. Mas \xE0s vezes\
  \ pode\u2026"
lastmod: '2024-03-25T19:22:45.846125-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar uma string geralmente significa converter o primeiro caractere\
  \ de uma string para mai\xFAscula e o restante para min\xFAscula. Mas \xE0s vezes\
  \ pode\u2026"
title: Capitalizando uma string
weight: 2
---

## O Que & Por Que?
Capitalizar uma string geralmente significa converter o primeiro caractere de uma string para maiúscula e o restante para minúscula. Mas às vezes pode significar apenas garantir que o primeiro caractere seja maiúsculo enquanto o restante da string permanece inalterado. Honestamente, na minha opinião, é um termo um tanto vago.

## Como Fazer:
Ruby oferece [métodos diretos para a manipulação de strings](https://docs.ruby-lang.org/en/3.3/String.html), incluindo capitalização:

```ruby
# Método embutido do Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Muito prático.

O método `.capitalize` do Ruby é conveniente, mas só coloca em maiúscula a primeira letra. Para ter mais controle ou para capitalizar cada palavra em uma string (conhecido como caso de título), você pode querer usar o método `titleize` da extensão ActiveSupport do Rails, ou implementá-lo por conta própria:

```ruby
# Usando 'titleize' do ActiveSupport no Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Uma solução caseira
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Este método divide a string em um array de palavras, capitaliza cada uma, e depois as une novamente com um espaço.

Pessoalmente, eu levo essa ideia muito mais longe no meu código. Eu escrevi meu próprio método [`titleize` que leva em conta palavras pequenas como "a" e "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
