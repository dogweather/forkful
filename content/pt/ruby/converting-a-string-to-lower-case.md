---
title:                "Convertendo uma string para minúsculas"
html_title:           "Fish Shell: Convertendo uma string para minúsculas"
simple_title:         "Convertendo uma string para minúsculas"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## O quê e Por quê?

Converter uma string para letras minúsculas é uma ação comum no processamento de texto em programação. Normalmente, é feito para garantir a consistência ao comparar ou manipular strings.

## Como fazer:

Vamos usar o método .downcase em Ruby para converter todas as letras de uma string para minúsculas. Aqui está um exemplo:

```Ruby
nome = "SEU NOME"
nome_minusculo = nome.downcase
puts nome_minusculo # Exibirá "seu nome"
```

## Deep Dive

A necessidade de converter strings para minúsculas remonta aos primeiros dias da computação. É uma forma de normalizar os dados de entrada para evitar erros causados por variações indesejadas na capitalização.

Existem algumas alternativas para o método .downcase em Ruby. Por exemplo, podemos usar o método .lowercase mas ele não é tão comumente usado. Outra opção é utilizar expressões regulares, no entanto, .downcase é normalmente a opção mais eficiente.

Por baixo dos panos, o método .downcase no Ruby verifica cada caractere da string. Se o caractere for uma letra maiúscula, ele a substitui pela mesma letra, mas em minúscula. 

## Veja também:

Para saber mais sobre o processamento de strings em Ruby, confira os seguintes links:

- [Ruby Doc - Downcase](https://ruby-doc.org/core-2.7.0/String.html#method-i-downcase)
- [Ruby Guides - Strings](https://www.rubyguides.com/2017/07/ruby-strings/)