---
title:                "Verificando se um diretório existe"
html_title:           "Clojure: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Verificar se um diretório existe é um procedimento em que checamos se um diretório específico já foi criado ou não. Os programadores fazem isso para evitar erros, como tentar acessar um diretório que não existe.

## Como Fazer:

A função `file-seq` retorna uma sequência preguiçosa das arquivos no diretório e em todos os subdiretórios. Para saber se um diretório existe, basta verificar se o código abaixo retorna uma sequência não nula. O código a seguir faz isso.

```Clojure
(defn directory-exists? [dir]
  (when-let [dirs (file-seq (clojure.java.io/file dir))]
    (not-empty (filter #(not (.isDirectory %)) dirs))))
```

E aqui está como você pode testá-lo:

```Clojure
(directory-exists? "/caminho/para/o/diretório")
```

## Mergulho Profundo

Historicamente, Clojure, assim como outras linguagens que funcionam na JVM, dependia do Java para interagir com o sistema de arquivos. A checagem da existência de um diretório, é, portanto, realizada de forma semelhante à sua contraparte em Java, usando a classe File do pacote java.io.

Como alternativa, você também pode usar um pacote do terceiro, como `me.raynes/fs`, que fornece funções mais Clojurianas para trabalhar com arquivos e diretórios.

Detalhes importantes da implementação: tenha em mente que essa abordagem não é atômica, e o estado do sistema de arquivos pode mudar entre o momento em que você verifica a existência do diretório e o momento em que você realmente acessa o diretório. Para algumas aplicações, isso pode ser preocupante e precisará ser tratado adequadamente.

## Veja Também

A documentação oficial de Clojure fornece uma descrição detalhada da biblioteca 'file-seq' em https://clojuredocs.org/clojure.java.io/file-dir.

Para mais informações sobre a biblioteca 'me.raynes/fs', consulte https://github.com/Raynes/fs.