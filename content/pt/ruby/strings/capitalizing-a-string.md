---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:08.633463-07:00
description: "Capitalizar uma string na programa\xE7\xE3o frequentemente se refere\
  \ a converter a primeira letra de uma string para mai\xFAscula e o restante para\
  \ min\xFAscula. Os\u2026"
lastmod: '2024-03-11T00:14:20.820279-06:00'
model: gpt-4-0125-preview
summary: "Capitalizar uma string na programa\xE7\xE3o frequentemente se refere a converter\
  \ a primeira letra de uma string para mai\xFAscula e o restante para min\xFAscula.\
  \ Os\u2026"
title: Capitalizando uma string
---

{{< edit_this_page >}}

## O Que & Por Que?
Capitalizar uma string na programação frequentemente se refere a converter a primeira letra de uma string para maiúscula e o restante para minúscula. Os programadores fazem isso por razões como aderir a convenções de nomenclatura, tornar saídas mais legíveis ou garantir consistência de dados para comparações e armazenamento.

## Como fazer:
Ruby oferece métodos diretos para manipulação de strings, incluindo capitalização. Aqui está como você pode capitalizar uma string em Ruby:

```ruby
# Método embutido do Ruby
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

O método `.capitalize` do Ruby é conveniente, mas só afeta a primeira letra. Para mais controle ou para capitalizar cada palavra em uma string (conhecido como caso de título), você pode querer usar o método `titleize` da extensão Rails ActiveSupport, ou implementá-lo você mesmo:

```ruby
# Usando 'titleize' do ActiveSupport no Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

Se você não está usando Rails ou prefere uma solução puramente em Ruby, aqui está como você pode capitalizar cada palavra em uma string:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Este método divide a string em um array de palavras, capitaliza cada uma, e então as junta de volta com um espaço.
