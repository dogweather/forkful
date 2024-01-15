---
title:                "Buscar e substituir texto"
html_title:           "Ruby: Buscar e substituir texto"
simple_title:         "Buscar e substituir texto"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que
Há muitas situações em que precisamos fazer alterações específicas em um texto. Seja corrigindo um erro de digitação ou atualizando informações, realizar buscas e substituições de texto é uma tarefa comum na programação.

## Como Fazer
Para realizar buscas e substituições de texto em Ruby, podemos usar o método `.gsub`. Este método recebe dois argumentos: o texto que queremos substituir e o texto que será usado como substituto. Por exemplo, se quisermos substituir todas as ocorrências de "cachorro" por "gato" em uma string, podemos fazer dessa forma:

```Ruby
texto = "Eu tenho um cachorro, ele se chama Max."
puts texto.gsub("cachorro", "gato")
```

Neste exemplo, a saída será: "Eu tenho um gato, ele se chama Max." Note que o método `.gsub` substitui todas as ocorrências encontradas.

Podemos também usar expressões regulares para realizar buscas mais precisas. Por exemplo, se quisermos substituir apenas as vogais por asteriscos, poderíamos utilizar o seguinte código:

```Ruby
texto = "Olá, mundo!"
puts texto.gsub(/[aeiou]/, "*")
```

A saída seria: "*lá, m*nd*!". Como podemos ver, o método `.gsub` funciona de forma muito versátil e pode ser utilizado de diversas maneiras para realizar buscas e substituições de texto.

## Deep Dive
O método `.gsub` é muito poderoso, mas é importante entender como ele funciona por baixo dos panos. Ele utiliza expressões regulares para encontrar as ocorrências do texto a ser substituído e, em seguida, utiliza o texto substituto para realizar a troca.

Além disso, é importante ressaltar que o método `.gsub` retorna uma nova string com as alterações realizadas, preservando a string original. Se quisermos modificar a string original, podemos utilizar o método `.gsub!`, com o ponto de exclamação no final.

## Veja Também
- [Documentação do método gsub](https://ruby-doc.org/core-2.6.3/String.html#method-i-gsub)
- [Expressões Regulares em Ruby](https://www.rubyguides.com/2015/06/ruby-regular-expressions/)