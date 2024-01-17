---
title:                "Concatenando strings"
html_title:           "Ruby: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/concatenating-strings.md"
---

{{< edit_this_page >}}

# O que & Por quê?

Concatenar strings é basicamente juntar duas ou mais strings, ou seja, cadeias de caracteres, em uma única string. 

Os programadores utilizam a concatenação de strings para criar novas strings a partir de informações diferentes, tornando-as mais dinâmicas e personalizadas. Além disso, o processo de concatenar strings é muito útil quando se trabalha com entradas do usuário, pois permite combinar essas entradas com outras informações e exibi-las de forma coerente.

# Como fazer:

```Ruby
# Utilizando o operador de adição (+)
"Olá" + " " + "mundo" #=> "Olá mundo"

# Utilizando o método concat
"Olá".concat(" ").concat("mundo") #=> "Olá mundo"

# Utilizando o método << (append)
"Olá" << " mundo" #=> "Olá mundo"
```

# Mergulho Profundo:

A concatenação de strings é uma operação muito antiga e amplamente utilizada na programação, tendo sido introduzida na linguagem de programação FORTRAN em 1954. Além dos métodos mencionados acima, existem outras formas de concatenar strings, como por exemplo o método `concat!`, que modifica a string original e adiciona a nova string ao seu final.

Outra opção é utilizar a interpolação de strings, que permite inserir valores de variáveis diretamente em uma string sem a necessidade de concatenar manualmente. Por exemplo:

```Ruby
nome = "João"
puts "Olá #{nome}, como vai?"
#=> Olá João, como vai?
```

# Veja também:

Para saber mais sobre concatenação de strings em Ruby, acesse a documentação oficial da linguagem: https://ruby-doc.org/core-3.0.2/String.html#method-i-2B-2B

Também é possível utilizar o método `concat` em objetos do tipo Array para combinar seus elementos em uma única string: https://ruby-doc.org/core-3.0.2/Array.html#method-i-empty-3F