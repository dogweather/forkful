---
title:                "Encontrando o comprimento de uma string."
html_title:           "Ruby: Encontrando o comprimento de uma string."
simple_title:         "Encontrando o comprimento de uma string."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Por que
A obtenção do comprimento de uma string é uma tarefa comum em programação que permite aos desenvolvedores manipular e lidar com dados de forma eficiente. Saber como encontrar o comprimento de uma string é uma habilidade fundamental em Ruby e pode ser aplicada em uma variedade de projetos e aplicações.

## Como fazer
Para encontrar o comprimento de uma string em Ruby, podemos usar o método `length` ou `size`. Ambos os métodos retornam o número de caracteres em uma string. Veja os exemplos abaixo:

```Ruby
string = "Olá, mundo!"
puts string.length
# Output: 12

puts string.size
# Output: 12
```

Também é possível usar o método `count` para contar o número de caracteres específicos em uma string. Por exemplo, se quisermos contar o número de letras "o" na string acima, podemos fazer da seguinte forma:

```Ruby
puts string.count("o")
# Output: 2
```

Além disso, o método `chars` pode ser usado para converter uma string em um array contendo cada caractere individual. Em seguida, podemos usar o método `size` para determinar o número de elementos no array, que corresponde ao número de caracteres na string.

```Ruby
string = "Hello, world!"
array = string.chars
puts array.size
# Output: 13
```

## Mergulho profundo
Ao contrário de outras linguagens de programação, em Ruby, uma string é vista como um objeto. Isso significa que ela possui métodos próprios que podem ser usados para manipular e obter informações sobre a string. Para encontrar o comprimento de uma string em Ruby, o método `length` é aplicado ao objeto string, ao invés de ser uma função separada.

Além disso, é importante notar que o método `length` retorna o número de caracteres na string, enquanto o método `size` retorna o número de bytes. Isso pode ser útil ao lidar com strings multibyte, como caracteres acentuados em português.

## Veja também
- [Documentação oficial do método length em Ruby](https://ruby-doc.org/core-3.0.1/String.html#method-i-length)
- [Documentação oficial do método count em Ruby](https://ruby-doc.org/core-3.0.1/String.html#method-i-count)
- [Tutorial interativo sobre strings em Ruby](https://www.learnruby.org/en/strings)