---
title:                "Interpolando uma string"
html_title:           "Clojure: Interpolando uma string"
simple_title:         "Interpolando uma string"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

Introdução
O que é a interpolação de string e por que os programadores a utilizam são duas questões importantes a serem respondidas neste artigo. Comecemos por definir o que é a interpolação de string.

## O que e por que?
A interpolação de string é uma técnica usada pelos programadores para combinar strings e variáveis em uma única saída. Isso torna o código mais dinâmico e flexível, facilitando a leitura e manutenção do mesmo.

## Como fazer:
Para utilizar a interpolação de string em Clojure, basta usar o símbolo `~` seguido da variável ou expressão que deseja interpolar dentro de uma string delimitada por aspas duplas (`" "`). O resultado será a string original com os valores das variáveis substituídos.

Exemplo:
```Clojure
(def nome "João")
(def idade 30)
(str "Meu nome é ~nome e tenho ~idade anos.") 
```

Output:
`"Meu nome é João e tenho 30 anos."`

## Detalhes avançados:
A interpolação de string é comumente conhecida como uma forma mais concisa de "concatenar" strings e variáveis. Antes dessa técnica, os programadores costumavam usar a função `str` para unir cada parte da string e variáveis, o que tornava o código mais verboso e difícil de ler.

Além disso, é importante mencionar que a interpolação de string só funciona com aspas duplas, e não com aspas simples. Também é possível utilizar expressões mais complexas dentro da `~`, desde que a última expressão seja precedida por `~`.

Exemplo:
```Clojure
(def nome "Maria")
(def sobrenome "Silva")
(def idade 25)
(str "Meu nome completo é ~nome ~sobrenome e minha idade é ~(+ idade 5) anos.") 
```

Output:
`"Meu nome completo é Maria Silva e minha idade é 30 anos."`

## Veja também:
- Documentação oficial de Clojure sobre interpolação de string: https://clojure.org/guides/weird_characters
- Outras técnicas para combinar strings em Clojure: https://clojuredocs.org/clojure.core/map/join 
- Aprenda mais sobre Clojure e suas principais funcionalidades: https://clojure.org/