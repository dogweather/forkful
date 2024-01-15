---
title:                "Maiúsculas em uma string"
html_title:           "Clojure: Maiúsculas em uma string"
simple_title:         "Maiúsculas em uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Porque

Se você já se deparou com a tarefa de capitalizar uma string em Clojure, pode ter se perguntado se realmente vale a pena. Afinal, por que alguém perderia tempo fazendo isso? A resposta é simples: a capitalização de strings é uma técnica comum em programação que pode facilitar o trabalho com dados e ajudar a obter resultados mais precisos.

## Como Fazer

Abaixo, você encontrará um exemplo de código que mostra como capitalizar uma string usando a função "capitalize" em Clojure:

```Clojure
(def string "exemplo de texto") ;; definindo uma string
(capitalize string) ;; saída: "Exemplo de texto"
```

O código acima é bem simples. Primeiro, definimos uma string como "exemplo de texto". Em seguida, usamos a função "capitalize" para capitalizar a primeira letra de cada palavra na string. O resultado é a string "Exemplo de texto".

É importante notar que a função "capitalize" só funciona com strings em letras minúsculas. Se você tentar capitalizar uma string que já está toda em maiúsculas, a função não terá efeito. Além disso, lembre-se de que a função irá apenas capitalizar a primeira letra de cada palavra, não todas as letras. Por exemplo:

```Clojure
(def string "exemplo DE texto") ;; definindo uma string
(capitalize string) ;; saída: "Exemplo DE texto"
```

Por fim, se você estiver trabalhando com uma string que contém acentos ou caracteres especiais, a função "capitalize" pode não funcionar corretamente. Nesses casos, é importante verificar se a string está codificada corretamente antes de tentar capitalizá-la.

## Mergulho Profundo

A função "capitalize" é apenas uma das várias maneiras de capitalizar uma string em Clojure. Existem também outras funções, como "capitalize-first" e "title-case", que podem oferecer resultados diferentes. Além disso, é possível também criar sua própria função personalizada para atender às suas necessidades específicas.

Outro aspecto importante a se considerar é que a capitalização de strings pode ser sensível ao idioma. Por exemplo, se você estiver trabalhando com o idioma português, a função "capitalize" pode não funcionar corretamente com palavras que contêm letras acentuadas. Nesses casos, é recomendado utilizar uma função específica para tratar esses casos, como "capitalize-locales".

Portanto, é sempre bom pesquisar e testar diferentes opções antes de decidir qual método de capitalização é o mais adequado para sua situação.

## Veja Também

- [Documentação oficial Clojure sobre strings](https://clojure.org/guides/strings)
- [Exemplo de uso da função "capitalize"](https://www.baeldung.com/clojure-capitalize-string)
- [Artigo explicando as diferenças entre as diferentes funções de capitalização em Clojure](https://www.jakewoods.me/blog/post/Clojure-Capitalization)