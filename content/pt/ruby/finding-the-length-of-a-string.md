---
title:                "Ruby: Encontrando o comprimento de uma string"
simple_title:         "Encontrando o comprimento de uma string"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Você já se perguntou como os programadores conseguem encontrar o comprimento de uma string em seus códigos? Descubra a importância dessa habilidade e como você também pode aplicá-la em seus projetos Ruby.

## How To
Para encontrar o comprimento de uma string em Ruby, podemos usar o método .length. Você simplesmente precisa chamar o método na string e ele retornará o número de caracteres. Veja um exemplo abaixo:

```Ruby
string = "Olá, mundo!"
puts string.length
```

A saída desse código será 12, já que a string possui 12 caracteres. Mas e se tivermos espaços em branco no início ou fim da string? Nesse caso, podemos usar o método .strip antes de .length para remover esses espaços e ter uma contagem precisa. Veja:

```Ruby
string = "  Olá, mundo!  "
puts string.strip.length
```

Agora a saída será 12 novamente, pois removemos os espaços antes de usar o método .length. Podemos também combinar esses dois métodos em uma única linha de código:

```Ruby
string = "   Olá, mundo!   "
puts string.strip.length
```

Assim, teremos a mesma saída sem a necessidade de criar uma variável.

## Deep Dive
Vamos dar uma olhada mais aprofundada no método .length. Ele pode ser aplicado em vários tipos de dados, como strings, arrays e hashes. Em strings, ele contabiliza o número de caracteres, mas em arrays e hashes, ele retornará o número de elementos.

Além disso, é importante lembrar que o método .length é diferente do método .size. Enquanto .length conta apenas os caracteres, .size contabiliza também os elementos em branco. Por exemplo, se tivermos uma string com dois espaços em branco, o método .length retornará 2, enquanto .size retornará 4.

Além disso, podemos usar o método .bytesize para encontrar o tamanho em bytes de uma string, considerando também os caracteres especiais e acentos.

## See Also
Para mais informações sobre como trabalhar com strings em Ruby, confira os links abaixo:

- Documentação oficial do método .length: https://ruby-doc.org/core-2.7.2/String.html#method-i-length
- Diferença entre .length e .size: https://stackoverflow.com/questions/2754313/what-is-the-difference-between-count-length-size-in-ruby
- Diferença entre .length e .bytesize: https://stackoverflow.com/questions/9655419/ruby-whats-the-difference-between-size-and-length-for-a-string/9655552