---
title:                "Convertendo uma string para minúsculas."
html_title:           "Ruby: Convertendo uma string para minúsculas."
simple_title:         "Convertendo uma string para minúsculas."
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Por que

Conversão de string para minúscula é uma operação comum em programação, especialmente quando se lida com entrada de dados do usuário ou comparação de strings. É importante saber como realizar essa conversão de forma eficiente e poderosa com Ruby.

## Como fazer

``` Ruby
# Usando o método downcase:
string = "OLA MUNDO"
puts string.downcase
#Output: ola mundo

# Usando o método map:
string = "OLA MUNDO"
puts string.chars.map(&:downcase).join
# Output: ola mundo

# Usando o método gsub:
string = "OLA MUNDO"
puts string.gsub(/[A-Z]/) {|match| match.downcase}
# Output: ola mundo
```

## Profundidade

Ao converter uma string para minúscula com o método downcase, todas as letras serão convertidas para minúsculas, incluindo letras acentuadas e caracteres especiais. Isso é importante ao lidar com diferentes idiomas e conjuntos de caracteres. Além disso, o método downcase também possui a variante downcase!, que modifica a string original, em vez de retornar uma nova. Isso pode ser útil para economizar memória ao lidar com strings grandes. Outra opção para converter para minúsculas é utilizar o método map, que funciona transformando cada caractere da string em um array, aplicando o downcase em cada um e, por fim, unindo-os novamente em uma string. O método gsub também é uma opção viável, substituindo caracteres em uma string que correspondam a uma determinada expressão regular por outro valor, neste caso, uma versão em minúsculas do próprio caractere. 

## Veja também

- Documentação da Classe "String": https://ruby-doc.org/core-3.0.1/String.html 
- Ruby Guides - Manipulação de Strings: https://www.rubyguides.com/ruby-tutorial/string-methods/ 
- Downcase vs lowercase in Ruby: https://stackoverflow.com/questions/9452031/downcase-vs-lowercase-in-ruby