---
title:                "Clojure: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração?

Imprimir uma saída de depuração é uma prática comum em programação, especialmente quando se trata de descobrir problemas no código. Isso pode ajudar a encontrar erros e rastrear o fluxo do programa, o que torna a depuração mais fácil e eficiente.

## Como fazer

A impressão da saída de depuração pode ser feita usando a função `println` em Clojure. Um exemplo simples é:

```Clojure
(defn somar [a b]
  (println "Somando" a "e" b)
  (+ a b))
```

Isso imprimirá a mensagem "Somando 2 e 5" antes de retornar o resultado da soma 7. Além disso, pode-se usar a função `pr` para imprimir valores de forma mais legível.

## Mergulho profundo

É importante notar que imprimir saída de depuração deve ser usado com moderação, pois pode causar poluição visual e tornar o código mais difícil de ler. Além disso, é recomendável usar ferramentas de depuração específicas, como o REPL, para encontrar e corrigir erros.

No entanto, se a saída de depuração for necessária, a função `println` também pode ser usada para imprimir valores de variáveis e expressões complexas, ajudando a entender como o código está sendo executado. Além disso, é possível usar a diretiva `#dbg` para imprimir informações adicionais, como o tempo de execução de uma função.

## Veja também

- [Documentação oficial sobre depuração em Clojure](https://clojure.org/guides/repl/debugging)
- [Tutorial sobre depuração em Clojure](https://purelyfunctional.tv/guide/debugging-clojure/)
- [Lista de ferramentas de depuração para Clojure](https://clojure.org/community/tools#_debugging_and_troubleshooting)