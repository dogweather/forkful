---
title:                "Capitalizando uma String"
date:                  2024-03-25T17:31:45.834951-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Porquê?
Capitalizar uma string geralmente significa converter o primeiro caráter de uma string para maiúscula e o resto para minúscula. Mas, às vezes, pode significar apenas garantir que o primeiro caráter seja maiúsculo enquanto o resto da string permanece inalterado. Honestamente, na minha opinião, é um termo um tanto vago.

## Como fazer:
O Ruby oferece [métodos diretos para a manipulação de strings](https://docs.ruby-lang.org/en/3.3/String.html), incluindo a capitalização:

```ruby
# Método embutido do Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Muito prático.

O método `.capitalize` do Ruby é conveniente, mas apenas transforma em maiúscula a primeira letra. Para ter mais controle ou para capitalizar cada palavra de uma string (conhecido como caso de título), você pode querer usar o método `titleize` da extensão ActiveSupport do Rails, ou implementá-lo você mesmo:

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

Este método divide a string em um array de palavras, capitaliza cada uma e depois as une novamente com um espaço.

Pessoalmente, eu levo essa ideia muito mais longe no meu código. Eu escrevi meu próprio método [`titleize` que leva em conta palavras pequenas como "a" e "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
