---
title:    "Clojure: Gerando um arquivo temporário"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Clojure?

Criar arquivos temporários é uma prática comum em muitas linguagens de programação, incluindo Clojure. Esses arquivos são úteis quando precisamos armazenar informações temporárias durante a execução de um programa, como dados de cache ou logs.

## Como criar um arquivo temporário em Clojure

Para criar um arquivo temporário em Clojure, podemos usar a função `with-open`. Ela aceita dois argumentos: um path (caminho) para o arquivo e uma função que manipula esse arquivo.

```
(defn create-temp-file []
  (with-open [temp-file (java.io.File/createTempFile "prefix-" ".log")]
    (println "Arquivo temporário criado:" (.getName temp-file))))
```

O código acima criará um arquivo temporário com o prefixo "prefix-" e a extensão ".log". A função `createTempFile` retorna um objeto File, que é então manipulado pela função dentro do `with-open`. No exemplo acima, apenas imprimimos o nome do arquivo temporário, mas podemos fazer qualquer operação que precisarmos dentro da função.

Ao final da execução do programa, o arquivo temporário será automaticamente excluído.

## Mais informações sobre a criação de arquivos temporários

Em Clojure, podemos usar diferentes funções para criar arquivos temporários, como `temporary-file` e `temporary-file` do pacote `clojure.java.io`.

Além disso, podemos especificar o local onde queremos que o arquivo temporário seja criado, passando um terceiro argumento para a função `createTempFile` com o caminho desejado.

## Veja também

- [Documentação oficial da função createTempFile](https://docs.oracle.com/javase/8/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-)
- [Pacote clojure.java.io](https://clojuredocs.org/clojure.java.io)