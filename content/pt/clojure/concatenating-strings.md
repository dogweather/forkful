---
title:                "Concatenando strings"
html_title:           "Clojure: Concatenando strings"
simple_title:         "Concatenando strings"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Por que

Você já precisou juntar mais de uma string em um único resultado de texto? A concatenação de strings é uma tarefa comum e útil em muitos cenários de programação, como na criação de mensagens personalizadas, formatação de dados e construção de URLs.

## Como Fazer

A concatenação de strings em Clojure é feita usando a função `str`, que aceita uma ou mais strings como argumentos e retorna uma string contendo a junção desses valores. Vamos ver alguns exemplos:

```
(str "Olá" ", " "mundo!")
; => "Olá, mundo!"

(str "1 + 1 = " (+ 1 1))
; => "1 + 1 = 2"
```

Podemos também usar a função `format` para formatar uma string com valores específicos:

```
(format "Meu nome é %s e eu tenho %d anos." "João" 30)
; => "Meu nome é João e eu tenho 30 anos."
```

Também é possível usar a função `concat` para juntar uma lista de strings em uma única string:

```
(concat ["Esta" " é" " uma" " string"])
; => "Esta é uma string"
```

## Mergulho Profundo

Ao contrário de outras linguagens de programação, em que a concatenação de strings pode ser feita usando o operador `+`, em Clojure é recomendado o uso da função `str` para garantir um desempenho otimizado. Isso ocorre porque, ao usar o operador `+`, novas strings são criadas em cada iteração do loop, o que pode levar a problemas de desempenho em casos de concatenação de muitas strings.

Além disso, vale ressaltar que, em Clojure, strings são imutáveis, ou seja, não podem ser alteradas. Isso significa que cada vez que uma operação de concatenação é feita, uma nova string é criada e a antiga é descartada. Portanto, é importante ter cuidado ao fazer muitas operações de concatenação em um loop, pois pode resultar em consumo excessivo de memória.

## Veja Também

- Documentação oficial da função `str`: https://clojuredocs.org/clojure.core/str
- Comparação de desempenho entre `str` e `+`: https://complexcarbohydrate.org/str-vs-nightmare-of-combinator{*em inglês}

Esperamos que este artigo tenha sido útil para você entender como fazer concatenação de strings em Clojure. Experimente as funções apresentadas e explore outras maneiras de manipular strings na sua programação!