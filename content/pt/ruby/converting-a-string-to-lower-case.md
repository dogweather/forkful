---
title:                "Ruby: Convertendo uma string para minúsculas."
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Ao trabalhar com strings em programação, muitas vezes é necessário formatar uma string para que ela esteja em letras minúsculas. Isso pode ser útil ao comparar strings, pois o Ruby é case-sensitive, ou seja, diferencia letras maiúsculas de minúsculas. Além disso, é mais fácil de ler e manipular strings em letras minúsculas.

## Como fazer

```Ruby
string = "Olá MUNDO"
puts string.downcase
```

Saída: "olá mundo"

Para converter uma string para letras minúsculas em Ruby, utilizamos o método `downcase`. Este método retorna uma cópia da string original com todas as letras em minúsculo.

Também é possível utilizar o método `downcase!`, com o ponto de exclamação no final, para modificar a string original em vez de retornar uma cópia. Por exemplo:

```Ruby
string = "HELLO WORLD"
string.downcase! #modifica a string original
puts string #saída: "hello world"
```

## Profundidade

O método `downcase` é sensível a caracteres acentuados em português, o que significa que ele irá convertê-los para suas respectivas versões em letras minúsculas. Por exemplo:

```Ruby
string = "ÁRVORE"
puts string.downcase
```

Saída: "árvore"

Portanto, é importante estar ciente disso ao utilizar esse método em strings que contenham caracteres acentuados.

Além disso, o método `downcase` não afeta outros caracteres que não sejam letras. Por exemplo:

```Ruby
string = "123 OLÁ MUNDO!"
puts string.downcase
```

Saída: "123 olá mundo!"

## Veja também

- [Ruby Documentation: Strings](https://ruby-doc.org/core-2.7.2/String.html)
- [Tutorialspoint: Ruby Strings](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Codecademy: Manipulando Strings em Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-string-manipulation-u/cheatsheet)