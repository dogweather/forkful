---
title:    "Clojure: Imprimindo saída de depuração"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que imprimir saída de depuração em Clojure?

 A impressão de saída de depuração é uma ferramenta essencial para a resolução de problemas e a compreensão do fluxo de dados em um programa Clojure. Ao imprimir informações específicas durante a execução de um código, podemos detectar erros, entender a lógica do programa e melhorar a eficiência do código.

## Como fazer

Aqui estão alguns exemplos de como podemos imprimir saída de depuração em nossos programas Clojure:

```Clojure
(let [x 10
      y 20]
  (println "O valor de x é:" x)
  (println "O valor de y é:" y))
```

**Saída:**

```
O valor de x é: 10
O valor de y é: 20
```

Podemos também usar a função `prn` para imprimir a representação em texto de uma expressão:

```Clojure
(prn (+ 2 3))
```

**Saída:**

```
5
```

E para imprimir informações de uma estrutura de dados, como uma lista ou mapa, podemos usar a função `pprint`:

```Clojure
(pprint [1 2 3 4 5])
```

**Saída:**

```
[1 2 3 4 5]
```

## Aprofundando-se

Além das funções mencionadas acima, também podemos usar a macro `doseq` para imprimir informações de cada item de uma coleção:

```Clojure
(doseq [x (range 10)]
  (println "O valor de x é:" x))
```

**Saída:**

```
O valor de x é: 0
O valor de x é: 1
O valor de x é: 2
O valor de x é: 3
O valor de x é: 4
O valor de x é: 5
O valor de x é: 6
O valor de x é: 7
O valor de x é: 8
O valor de x é: 9
```

Uma dica útil para imprimir informações de depuração é usar a função `bindings` para ver os valores atuais das variáveis durante a execução do código. Por exemplo:

```Clojure
(let [x 10
      y (+ x 5)]
  (pprint (bindings)))
```

**Saída:**

```
{x 10, y 15}
```

## Veja também

- [Documentação oficial do Clojure sobre impressão de saída de depuração](https://clojuredocs.org/clojure.core/prn)
- [Tutorial de impressão de saída de depuração em Clojure](https://www.brainjar.com/clojure/introduction/logging)
- [Artigo sobre estratégias efetivas de depuração em Clojure](https://purelyfunctional.tv/guide/clojure-debugging/)