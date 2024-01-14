---
title:                "Clojure: Escrevendo para o erro padrão"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Por que escrever para o erro padrão?

Escrever para o erro padrão é uma habilidade importante para qualquer programador. Isso permite que você registre mensagens de erro e outras informações úteis durante a execução de um programa. Isso pode ajudar a localizar e corrigir problemas em seu código e melhorar a qualidade de seus programas.

## Como fazer

Para escrever para o erro padrão em Clojure, você pode usar a função "println", que irá imprimir a mensagem fornecida para o console. Você também pode usar a função "eprintln" para imprimir a mensagem com a indicação de erro padrão. Veja alguns exemplos abaixo:

```
Clojure
(println "Hello World!")
;; output: Hello World!

(def num 10)
(eprintln "O valor de num é" num)
;; output: O valor de num é 10 - com a indicação de erro padrão
```

## Profundidade

Agora que você sabe como escrever para o erro padrão em Clojure, vamos dar uma olhada em algumas informações mais avançadas sobre o assunto. Existem outras funções que você pode usar para lidar com as mensagens de erro, como "pr-str" para converter um valor em uma string e "pr" para imprimir um valor sem adicionar uma nova linha no final.

Além disso, você também pode personalizar a saída de erro padrão usando a macro "with-out-str", que captura a saída para uma string e permite que você a use de diferentes maneiras. Por exemplo:

```
Clojure
(with-out-str (pr "Hoje é" (java.util.Date.)))
;; output: Hoje é Tue Apr 07 10:10:26 PST 2020
```

Lembre-se de que é importante ter cuidado ao escrever para o erro padrão. Você não quer sobrecarregar o console com muitas informações, pois isso pode dificultar a leitura e a compreensão dos dados de saída.

## Veja também

- [Documentação oficial de Clojure sobre escrita para o erro padrão](https://clojuredocs.org/clojure.core/eprintln)
- [Tutorial em vídeo: Como escrever no erro padrão em Clojure](https://www.youtube.com/watch?v=8zJavuFePKQ)
- [Exemplos de uso do Clojure: Função "println"](https://github.com/clojure/clojure/blob/master/src/main/clojure/clojure/core.clj#L3236)