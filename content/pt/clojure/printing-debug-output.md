---
title:                "Saida de depuração de impressão"
html_title:           "Clojure: Saida de depuração de impressão"
simple_title:         "Saida de depuração de impressão"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## O Que e Por Que?

Debug output, ou saída de depuração, é a exibição de informações úteis durante a fase de desenvolvimento de um programa. Programadores utilizam isso para verificar o funcionamento do código e encontrar possíveis erros ou falhas. É uma técnica importante para garantir a qualidade do software.

## Como Fazer:

```Clojure
; Utilize a função println para imprimir mensagens de depuração
(println "Essa é uma mensagem de depuração")
; Saída: Essa é uma mensagem de depuração

; Você também pode imprimir valores de variáveis
(def x 10)
(println "O valor de x é:" x)
; Saída: O valor de x é: 10
```

## Mergulho Profundo:

A prática de imprimir debug output é comum há décadas e é amplamente utilizada em diferentes linguagens de programação. Além da função println, também existem outras formas de imprimir saída de depuração, como a função print ou a biblioteca tools.logging. Algumas ferramentas de desenvolvimento oferecem recursos mais avançados para imprimir e analisar a saída de depuração, como o REPL no Clojure.

## Veja Também:

- [Documentação oficial do println](https://clojuredocs.org/clojure.core/println)
- [Artigo sobre depuração em Clojure](https://clojure.org/guides/getting_started#debugging)
- [Ferramentas de desenvolvimento Clojure](https://clojure.org/community/resources)