---
title:                "Clojure: Imprimindo saída de depuração"
programming_language: "Clojure"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração?

Depurar erros e problemas em um código pode ser uma tarefa difícil e demorada. Imprimir saída de depuração é uma técnica simples e eficaz para ajudar os programadores a identificar e corrigir esses problemas de forma mais eficiente.

## Como fazer

Para imprimir saída de depuração em Clojure, podemos usar a função `println` ou `prn`. A diferença entre elas é que `println` adiciona uma nova linha no final da saída, enquanto `prn` não adiciona. Podemos usar essas funções em qualquer parte do nosso código, inserindo variáveis ou valores entre parênteses. Veja um exemplo abaixo:

```Clojure
(defn calcular-media [lista]
  (let [soma (apply + lista)]
    (println "A soma dos valores é:" soma)
    (let [media (/ soma (count lista))]
      (prn "A média dos valores é:" media)
      media)))

(def lista-numeros [10 15 20 25])
(def media-result (calcular-media lista-numeros))
(println "A média final é:" media-result)
```

A saída para esse código seria:

```
A soma dos valores é: 70
"A média dos valores é: 17.5"
A média final é: 17.5
```

## Mergulho profundo

Além da função `println` e `prn`, também podemos usar a função `print` para imprimir saída de depuração em Clojure. A diferença entre `print` e `println` é que `println` adiciona uma nova linha no final, enquanto `print` não adiciona. Além disso, podemos usar a função `str` para converter valores em sua forma de string antes de imprimi-los.

Outra técnica útil é usar a função `with-out-str` para capturar a saída de depuração em uma variável, em vez de imprimi-la diretamente. Isso pode ser útil para depurar funções que retornam strings grandes, por exemplo.

## Veja também

- [Documentação oficial sobre a função `println`](https://clojuredocs.org/clojure.core/println)
- [Documentação oficial sobre a função `prn`](https://clojuredocs.org/clojure.core/prn)
- [Documentação oficial sobre a função `print`](https://clojuredocs.org/clojure.core/print)
- [Documentação oficial sobre a função `str`](https://clojuredocs.org/clojure.core/str)