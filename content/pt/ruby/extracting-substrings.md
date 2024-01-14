---
title:                "Ruby: Extraindo subcadeias"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Por que?

A extração de substrings é uma ferramenta importante na programação Ruby, pois permite aos programadores manipular strings de uma forma mais eficiente. Com a extração de substrings, é possível obter partes específicas de uma string, o que pode ser útil em muitas situações diferentes.

## Como fazer

Para extrair uma substring em Ruby, pode-se usar o método `.slice` ou o operador `[]`. Ambos possuem a mesma funcionalidade e podem ser usados da seguinte forma:

```Ruby
# Exemplo usando .slice
string = "Hello world"
substring = string.slice(0, 5)
puts substring  # Output: Hello

# Exemplo usando operador []
string = "Hello world"
substring = string[6..-1]
puts substring  # Output: world
```

No primeiro exemplo, usamos o `.slice` para obter os primeiros 5 caracteres da string original, enquanto no segundo exemplo, usamos o operador `[]` para obter os caracteres a partir da posição 6 até o final.

Além disso, é possível usar métodos como `.length` e `.index` para obter informações sobre a string original e, assim, definir os parâmetros corretos para a extração da substring.

## Deep Dive

Ao usar o método `.slice` ou o operador `[]`, é preciso entender como os parâmetros funcionam. Seguindo a convenção de índices em Ruby, a contagem começa em 0 e não em 1. Além disso, pode-se usar números negativos para contar a partir do final da string.

Por exemplo, se quisermos obter a última palavra de uma string, podemos usar o operador `[]` desta forma: `string[-1]`. Isso também pode ser aplicado ao usar o método `.slice`.

Outro aspecto importante é que, ao usar `.slice` ou `[]` para obter uma substring, a string original não é alterada e uma nova string é retornada, o que pode ser útil para preservar a string original.

## Veja também
- [Ruby Documentation: String](https://ruby-doc.org/core-2.7.1/String.html)
- [Codecademy: Ruby String Methods](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-string-methods)
- [RubyMonk: The Ruby String Object](https://rubymonk.com/learning/books/1-ruby-primer/chapters/1-strings/lessons/2-the-ruby-string-object)