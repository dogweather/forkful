---
title:                "Criando um arquivo temporário"
html_title:           "Clojure: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que

Criar arquivos temporários pode ser útil em diversas situações, principalmente quando estamos lidando com processos de armazenamento temporário de dados ou quando precisamos realizar operações seguras e eficientes.

## Como Fazer

Para criar um arquivo temporário em Clojure, podemos utilizar a função `with-open` combinada com a função `tempfile`. Por exemplo, podemos criar um arquivo temporário nomeado "temp.txt" da seguinte maneira:

```Clojure
(with-open [file (tempfile "temp.txt")]
    (println "Criado arquivo temporário: " (.getAbsolutePath file)))
```

O código acima irá criar o arquivo temporário no diretório padrão do sistema operacional e imprimir o seu caminho absoluto. Além disso, o arquivo é automaticamente excluído após o encerramento do bloco `with-open`.

## Deep Dive

Por trás dos panos, a função `tempfile` utiliza a classe `java.io.File` e o método `createTempFile` para criar o arquivo temporário. Além disso, a função `with-open` gerencia automaticamente o fechamento do arquivo ao final do bloco de código.

Caso queiramos especificar um diretório diferente para a criação do arquivo temporário, podemos passar o caminho como parâmetro para a função `tempfile`, por exemplo: `(with-open [file (tempfile "C:/temp" "temp.txt")] ...)`

## Veja também

- [Documentação de tempfile em Clojure](https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/tempfile)
- [Tutorial oficial de Clojure](https://clojure.org/guides/getting_started)