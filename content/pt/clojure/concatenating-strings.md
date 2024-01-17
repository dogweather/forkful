---
title:                "Juntando strings"
html_title:           "Clojure: Juntando strings"
simple_title:         "Juntando strings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## O que e por que?

Concatenar strings é quando você combina uma ou mais strings para formar uma nova string. Isso é feito principalmente quando você deseja combinar informações diferentes para exibir uma mensagem mais compreensível ou para criar um novo valor de variável.

## Como fazer:

Aqui estão alguns exemplos de código para concatenar strings em Clojure:

```Clojure
(def primeira-string "Olá")
(def segunda-string "mundo!")
(def nova-string (str primeira-string " " segunda-string))
(println nova-string)
```

A saída seria: "Olá mundo!"

Você também pode usar a função `str` para adicionar várias strings juntas de uma vez. Por exemplo:

```Clojure
(def nome "João")
(def sobrenome "Silva")
(def idade 30)
(def novo-valor (str "Meu nome é " nome " " sobrenome " e eu tenho " idade " anos."))
(println novo-valor)
```

A saída seria: "Meu nome é João Silva e eu tenho 30 anos."

## Mergulho profundo:

Concatenar strings é uma técnica muito comum na programação e tem sido usada desde os primeiros dias da linguagem de programação. Outras linguagens também possuem métodos semelhantes para concatenar strings, como o `concat` do Python e o `+` do Java.

Em Clojure, a função `str` é uma função interna que usa uma implementação mais eficiente para concatenar strings do que simplesmente usar o operador `+`. Isso é feito através do uso de uma classe `StringBuilder`, que é otimizada para criar e manipular strings.

## Veja também:

- Documentação oficial de Clojure sobre a função `str`: https://clojuredocs.org/clojure.core/str
- Tutoriais sobre concatenação de strings em Clojure: https://www.braveclojure.com/working-with-strings/
- Tutorial sobre a classe `StringBuilder`: https://docs.oracle.com/javase/tutorial/java/data/buffers.html