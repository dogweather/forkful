---
title:                "Clojure: Capitalizando uma string"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Por que

Uma das tarefas fundamentais ao trabalhar com strings é capitalizá-las. Isso pode ser útil para deixar um texto mais legível ou para padronizar a formatação. Neste post, veremos como realizar essa tarefa usando a linguagem de programação Clojure.

## Como Fazer

Existem algumas maneiras diferentes de capitalizar uma string em Clojure, mas a forma mais simples é usando a função `clojure.string/capitalize`. Esta função recebe uma string como argumento e retorna a mesma string com a primeira letra em maiúscula. Vejamos um exemplo:

```Clojure
(clojure.string/capitalize "olá mundo")
```
```
"Olá mundo"
```

Podemos também usar essa função em conjunto com outras funções de manipulação de strings. Por exemplo, se quisermos capitalizar apenas a primeira palavra de uma frase, podemos usar a função `split-first` para separar a primeira palavra e então usar `capitalize` apenas nessa primeira palavra. Veja:

```Clojure
(clojure.string/join " " (cons (clojure.string/capitalize (first (clojure.string/split-first "olá mundo"))) (rest (clojure.string/split-first "olá mundo"))))
```
```
"Olá mundo"
```

## Mergulho Profundo

Agora que vimos como capitalizar uma string em Clojure, vamos dar uma olhada mais aprofundada no que está acontecendo por trás das cenas. A função `capitalize` usa o protocolo `StringProtocol` para manipular strings. Esse protocolo é implementado na biblioteca Java `java.lang.String`, o que significa que a função `capitalize` na verdade está utilizando a funcionalidade embutida do Java para realizar a capitalização da string.

Além disso, a função `capitalize` é apenas uma das muitas funções úteis disponíveis na biblioteca `clojure.string`. Vale a pena explorar essa biblioteca para conhecer outras formas de manipular strings em Clojure.

## Veja Também

- Documentação oficial de `clojure.string`: https://clojure.github.io/clojure/clojure.string-api.html
- Exemplos de uso de `clojure.string`: https://clojure.github.io/clojure/clojure.string-examples.html
- Outras funções úteis para manipular strings em Clojure: https://www.braveclojure.com/manipulating-strings/#Built%20in%20Toolkit