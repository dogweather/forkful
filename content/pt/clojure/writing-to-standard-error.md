---
title:    "Clojure: Escrevendo para o erro padrão"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-to-standard-error.md"
---

{{< edit_this_page >}}

Porque escrever para o erro padrão em Clojure é importante?

Quando estamos desenvolvendo em Clojure, é essencial que tenhamos uma forma de monitorar e diagnosticar possíveis erros que podem ocorrer durante a execução do programa. Escrever para o erro padrão nos permite acessar essas informações e entender melhor o que está acontecendo no nosso código.

Como fazer:

```Clojure
(defn erro-exemplo []
    (println "Executando função de exemplo")
    (throw (Exception. "Isso é um erro de exemplo")))
```

Ao executar a função acima, teremos o seguinte resultado no erro padrão:

```
Executando função de exemplo
Exception: Isso é um erro de exemplo
```

Podemos também gravar o erro padrão em um arquivo para utilizá-lo posteriormente para debug:

```Clojure
(defn erro-arquivo []
    (with-open [writer (io/writer "erros.txt")]
        (binding [*out* writer]
            (println "Escrevendo erro no arquivo!")
            (throw (Exception. "Isso é um erro simulado")))))
```

Desta forma, o erro será gravado no arquivo "erros.txt" e poderemos acessá-lo para análise.

Deep Dive:

Caso queiramos uma forma mais personalizada de lidar com os erros e suas informações, podemos utilizar a função `prn` para imprimir uma estrutura de dados no erro padrão, ao invés de exibir apenas a mensagem de erro:

```Clojure
(defn erro-estrutura []
    (prn {:mensagem "Erro personalizado" :numero 404 :função "Erro estruturado"}))
```

Isso irá produzir a seguinte saída no erro padrão:

```
{:mensagem "Erro personalizado" :numero 404 :função "Erro estruturado"}
```

Podemos até mesmo utilizar `prn` dentro de um `try/catch` para capturar e tratar o erro de forma mais controlada:

```Clojure
(defn erro-tratado []
    (try
        (throw (Exception. "Erro simulado"))
        (catch Exception e
            (prn {:mensagem "Erro tratado!" :erro e}))))
```

Neste caso, a saída no erro padrão será:

```
{:mensagem "Erro tratado!" :erro #object[java.lang.Exception 0x4b739ae4 "Erro simulado"]}
```

Isso nos permite ter um controle mais preciso sobre os erros que ocorrem em nosso código.

Veja também:

- [Visão geral da função `println` em Clojure](https://clojure.org/guides/io)
- [Documentação oficial de `prn` em Clojure](https://clojuredocs.org/clojure.core/prn)
- [Artigo sobre tratamento de erros em Clojure](https://hackernoon.com/exception-handling-in-clojure-4a8911e93a3d)

## Veja também:

- [Documentação oficial de `io/writer` em Clojure](https://clojuredocs.org/clojure.java.io/writer)
- [Explicação mais detalhada sobre a função `throw` em Clojure](https://clojuredocs.org/clojure.core/throw)
- [Exemplos de código para lidar com erros em Clojure](https://medium.com/@haiderkhantcs/handling-errors-in-clojure-8e1867f7b083)