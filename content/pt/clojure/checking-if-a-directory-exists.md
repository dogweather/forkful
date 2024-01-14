---
title:                "Clojure: Verificando se um diretório existe"
simple_title:         "Verificando se um diretório existe"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que verificar se um diretório existe?

Ao escrever um código em Clojure, é comum nos depararmos com a necessidade de verificar a existência de um diretório antes de executar determinadas ações. Isso pode ser útil para garantir que o diretório e os arquivos necessários estejam presentes e acessíveis antes de realizar operações de leitura ou gravação. Neste artigo, iremos explorar como verificar se um diretório existe em Clojure e como lidar com essa situação.

## Como fazer:

Existem várias maneiras de verificar a existência de um diretório em Clojure. Uma delas é utilizando a função `file-seq`, que retorna uma sequência de objetos correspondentes a cada arquivo e diretório em uma determinada raiz. Podemos combinar essa função com a função `some` para verificar se uma condição é verdadeira para algum elemento da sequência. Veja o exemplo abaixo:

```Clojure
(require '[clojure.java.io :as io])

(defn dir-exists? [dir]
  (some #(= dir (.getName %)) (file-seq (io/file "."))))

(dir-exists? "pasta") ; => true
(dir-exists? "arquivo.txt") ; => false
```

O código acima verifica se o diretório "pasta" existe na pasta atual. Se o diretório for encontrado, a função `dir-exists?` retorna `true`, caso contrário, retorna `false`.

Outra forma de verificar a existência de um diretório é utilizando a função `file`, que cria um objeto `java.io.File` a partir de uma string que representa o caminho do arquivo ou diretório. Podemos então utilizar a função `exists?` para verificar se o diretório existe. Veja o exemplo abaixo:

```Clojure
(require '[clojure.java.io :as io])

(defn dir-exists? [dir]
  (.exists? (io/file dir)))

(dir-exists? "pasta") ; => true
(dir-exists? "arquivo.txt") ; => false
```

## Profundidade:

Ao verificar a existência de um diretório, é importante considerar se o caminho especificado é absoluto ou relativo. Um caminho absoluto é o caminho completo a partir do diretório raiz do sistema operacional, enquanto um caminho relativo é o caminho a partir do diretório atual. Em Clojure, podemos utilizar a função `abspath` para transformar um caminho relativo em absoluto.

Outro ponto a se considerar é que, mesmo que um diretório exista, pode haver permissões de acesso que impeçam o programa de acessá-lo. Nesse caso, será necessário utilizar funções para manipular permissões de arquivos, como `set-file-mode`, `set-file-owner` e `set-file-group-owner`.

## Veja também:

[A Clojure filesequece de solutions](https://stuartsierra.com/2010/01/02/clojure-file-sequence-fileseq)
[Clojure docs - clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html)
[Clojure Cookbook - Verifying if a Path exists](https://clojure-cookbook.com/os/java.io.path/check_existence.html)