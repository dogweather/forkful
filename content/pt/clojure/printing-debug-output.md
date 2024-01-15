---
title:                "Imprimindo saída de depuração"
html_title:           "Clojure: Imprimindo saída de depuração"
simple_title:         "Imprimindo saída de depuração"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Por que

Você já teve dificuldade em entender porque seu código não está funcionando como esperado? Às vezes, pode ser difícil descobrir e entender exatamente o que está acontecendo em um programa. É aí que o recurso de impressão de saída de depuração pode ser útil.

## Como fazer

Para imprimir saída de depuração em Clojure, você pode usar a função `println` ou `prn`, que são incluídas na biblioteca padrão do Clojure. Aqui está um exemplo simples de como usá-las:

```Clojure 
(def num1 5) 
(def num2 10) 
(println "O valor de num1 é" num1) 
(prn "A soma de num1 e num2 é" (+ num1 num2))
```

A saída deste código será:

```
O valor de num1 é 5 
"A soma de num1 e num2 é" 15
```

Você também pode utilizar o recurso de formatação de strings do Clojure para personalizar a sua saída de depuração. Por exemplo:

```Clojure 
(def nome "Ana") 
(def sobrenome "da Silva") 
(println (format "Olá, meu nome é %s %s" nome sobrenome))
```

A saída deste código será:

```
Olá, meu nome é Ana da Silva
```

## Mergulho profundo

Além das funções `println` e `prn`, o Clojure também possui a função `print`, que funciona de maneira semelhante, mas não adiciona uma nova linha no final da saída. Você também pode usar a macro `dbg` para imprimir informações de depuração somente quando uma determinada condição é atendida, o que pode ser útil para evitar uma sobrecarga de saída de depuração.

Outra técnica útil para imprimir saída de depuração é utilizar o `clojure.pprint`, que permite que você formate de forma mais específica a sua saída, como por exemplo, a largura máxima da linha ou o número de dígitos após a vírgula.

Utilizar a impressão de saída de depuração também pode ser útil para entender a ordem de execução de funções e o valor de variáveis ​​em um programa mais complexo. Por isso, não tenha medo de utilizá-la sempre que necessário para ajudar a solucionar problemas no seu código.

## Veja também

- [Documentação do Clojure sobre a saída de depuração](https://clojure.org/reference/debugging)
- [Tutorial sobre como utilizar formatação de strings no Clojure](https://purelyfunctional.tv/guide/clojure-string-formatting/)