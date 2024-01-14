---
title:                "Ruby: Buscando e substituindo texto"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

Imagine que você tenha um texto gigante e precise fazer várias substituições em palavras específicas. Fazer isso manualmente pode ser uma tarefa árdua e demorada. Felizmente, com a programação em Ruby, podemos automatizar esse processo e economizar muito tempo e esforço.

## Como fazer

Em Ruby, podemos usar o método `gsub` para buscar e substituir texto em uma string. Ele recebe dois argumentos: o trecho de texto que desejamos substituir e o novo texto que será colocado no lugar. Vamos ver um exemplo:

```Ruby
texto = "Eu amo programar em Ruby!"
novo_texto = texto.gsub("amo", "adorei") #substitui "amo" por "adorei"
puts novo_texto
```

O output desse código seria: `Eu adorei programar em Ruby!`

Podemos usar o método `gsub` para substituir várias ocorrências de uma palavra em um texto, usando uma expressão regular como primeiro argumento. Por exemplo:

```Ruby
texto = "Aprendendo a programar em Ruby é muito divertido!"
novo_texto = texto.gsub(/[aA]/, "e") #substitui "a" e "A" por "e"
puts novo_texto
```

Output: `Erprendendo e progrermer em Ruby é muito divertido!`

## Deep Dive

O método `gsub` também é conhecido como "global substitution" por ser capaz de substituir todas as ocorrências de um texto em uma string. No entanto, ele também possui uma variante, o `sub`, que substitui a primeira ocorrência encontrada e depois para de procurar. Por exemplo:

```Ruby
texto = "Muitos programadores amam Ruby"
novo_texto = texto.sub("amam", "detestam") #substitui apenas a primeira ocorrência
puts novo_texto
```

Output: `Muitos programadores detestam Ruby`

Lembrando que tanto o `gsub` quanto o `sub` não alteram a string original, apenas retornam uma nova string com as substituições.

## Veja também

- [Documentação oficial do método `gsub` em Ruby](https://ruby-doc.org/core-2.7.3/String.html#method-i-gsub)
- [Tutorial sobre expressões regulares em Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Artigo sobre substituição de texto em Ruby](https://www.codegrepper.com/code-examples/ruby/ruby+gsub)