---
title:                "Extraindo substrings"
html_title:           "Bash: Extraindo substrings"
simple_title:         "Extraindo substrings"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## O Que e Por Quê?

Extrair substrings é o processo de obter uma parte menor de uma string existente. Programadores fazem isso para manipular, analisar ou alterar dados específicos de uma grande string.

## Como fazer:

No Ruby, o método mais comum de extração de substrings é 'slice'. Aqui está como você pode usá-lo:

```Ruby
str = "Olá, mundo!"
puts str.slice(0,5) # => "Olá, "
puts str.slice(7,5) # => "mundo"
```
'slice' pode receber dois argumentos: o índice inicial e o número de caracteres a extrair.

Além disso, também podem ser usados índices negativos, que contam a partir do fim da string.

```Ruby
puts str.slice(-1)   # => "!"
puts str.slice(-6, 5) # => "mundo"
```

## Mergulho Profundo

A extração de substrings surgiu como uma necessidade para lidar com grandes conjuntos de dados e manipular partes específicas desses dados para análises variadas. Alternativas ao 'slice' incluem o uso de 'substring' ou 'index' acompanhado por 'length', embora 'slice' seja geralmente preferido por ser mais conciso.

Além disso, a implementação da extração de substrings é eficiente no Ruby, dado que as strings são imutáveis: ao invés de alterar a string original, novas strings são criadas, o que, geralmente, torna o processo rápido, especialmente para strings de tamanho moderado.

## Veja Também: 

- Documentação oficial de Ruby sobre Substrings: https://ruby-doc.org/core-2.6.1/String.html
- Estudo aprofundado sobre métodos de string em Ruby: https://www.tutorialspoint.com/ruby/ruby_strings.htm
- Guia prático de Ruby para manipulação de substrings: https://www.rubyguides.com/2015/03/ruby-strings/