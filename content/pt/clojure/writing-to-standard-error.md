---
title:                "Clojure: Escrevendo no erro padrão"
simple_title:         "Escrevendo no erro padrão"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão em Clojure?

Ao escrevermos código em Clojure, muitas vezes nos deparamos com situações em que precisamos ser notificados sobre possíveis erros ou problemas no nosso programa. Uma forma de fazer isso é escrever para o chamado "standard error", que é um canal de comunicação de saída de dados destinado a informar sobre erros e exceções. Neste post, vamos explorar como podemos escrever para o erro padrão em Clojure e por que isso pode ser útil.

## Como fazer?

Para escrever para o erro padrão em Clojure, podemos utilizar a função `println` seguida do caractere `>`:

```Clojure
(println> "Este é um exemplo de mensagem de erro")
```

Isso imprimirá a mensagem especificada seguida de `[stderr]` no console, indicando o canal de saída utilizado. Podemos também utilizar a função `eprintln`, que já inclui esse identificador na impressão:

```Clojure
(eprintln "Este é outro exemplo de mensagem de erro")
```

## Aprofundando no assunto

Além das funções `println` e `eprintln`, Clojure também oferece a possibilidade de escrever diretamente para o erro padrão utilizando a função `prn`:

```Clojure
(prn> "Este é um erro mais detalhado: " {:detalhe "Erro de tipo"})
```

Isso nos permite escrever informações mais complexas e estruturadas, como mapas e vetores, para o erro padrão.

Também é importante destacar que o uso de `println>`, `eprintln` e `prn>` não interrompem a execução do programa, permitindo que possamos continuar a execução do código mesmo após a impressão de erros.

## Veja também

- [Documentação do standard error em Clojure](https://clojure.org/reference/errors)
- [Tutorial de Clojure para iniciantes](https://github.com/fogus/clojure-from-the-ground-up)
- [Como lidar com erros e exceções em Clojure](https://purelyfunctional.tv/courses/exception-handling-in-clojure/)