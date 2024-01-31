---
title:                "Verificando se um diretório existe"
date:                  2024-01-19
html_title:           "Arduino: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"

category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Verificar a existência de um diretório permite saber se um caminho específico é válido e acessível no sistema de arquivos. Programadores fazem isso para evitar erros ao tentar ler ou escrever em diretórios que não existem.

## Como fazer:

Para verificar se um diretório existe em Clojure, vamos usar a função `java.io.File` para criar um objeto de arquivo e depois o método `.exists` para checar se ele existe. Simples assim.

```clojure
(import 'java.io.File)

(defn directory-exists? [path]
  (.exists (File. path)))

(println (directory-exists? "/path/to/dir")) ; troque "/path/to/dir" pelo diretório que deseja verificar
;; output: true ou false, dependendo se o diretório existe
```

## Mergulho Profundo

Historicamente, a verificação da existência de diretórios é um aspecto fundamental da interação com o sistema de arquivos, evitando erros comuns de leitura e escrita. Em Clojure, a interoperabilidade com Java torna essa tarefa simples através da Java NIO (New IO), que oferece uma forma moderna e mais versátil de realizar operações de I/O no Java.

Outro método seria usar a biblioteca `clojure.java.io` para casos mais complexos ou quando se deseja um código mais idiomático Clojure.

Alternativamente, você pode usar o namespace `clojure.java.io` com a função `file` em conjunto com `.exists`, que é mais Clojure-esque:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists? [path]
  (.exists (io/file path)))

(println (directory-exists? "/path/to/dir")) ; troque "/path/to/dir"
;; output: true ou false
```

A implementação de verificação de diretórios deve sempre levar em consideração fatores como permissões de leitura/escrita e se o caminho é um diretório ou um arquivo, algo que pode ser complementado por métodos como `.isDirectory`.

## Veja Também

- Clojure Documentation: https://clojure.org/
- Java NIO File API: https://docs.oracle.com/javase/8/docs/api/java/nio/file/package-summary.html
- Clojure for the Brave and True - IO in Clojure: https://www.braveclojure.com/iostreams/
