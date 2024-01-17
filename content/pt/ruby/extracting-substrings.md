---
title:                "Extraindo subcadeias de caracteres"
html_title:           "Ruby: Extraindo subcadeias de caracteres"
simple_title:         "Extraindo subcadeias de caracteres"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## O que & Por quê?
Extrair substrings é o processo de selecionar apenas parte de uma string maior. Isso é útil para manipular dados em seus projetos de programação e simplificar tarefas. Os programadores fazem isso para economizar tempo e tornar seu código mais eficiente.

## Como fazer:
```
# Considere a string abaixo como exemplo
str = "Exemplo de string"

# Para extrair uma substring específica, use o método slice com os índices desejados
str[8..13] # output: "de str"

# Você também pode usar o método slice com um único índice para extrair do índice até o final da string
str[8..-1] # output: "de string"

# Outro método útil é o substring, que permite definir o tamanho da substring
str.substring(8, 6) # output: "de str"
```

## Profundando:
Extrair substrings é uma técnica comum utilizada em muitas linguagens de programação. Foi introduzida pela primeira vez em linguagens como o BASIC e FORTRAN. Além do Ruby, outras linguagens como Java e Perl também possuem métodos para extrair substrings. Além disso, existem diferentes abordagens para extrair substrings, como usando expressões regulares.

## Veja também:
- [Documentação do método slice em Ruby](https://ruby-doc.org/core-2.7.1/String.html#method-i-slice)
- [Tutorial de expressões regulares em Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Lista de outras linguagens de programação que suportam extração de substrings](https://en.wikipedia.org/wiki/Substring#Computing)