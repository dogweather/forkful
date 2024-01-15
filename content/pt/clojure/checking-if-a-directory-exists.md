---
title:                "Verificando se um diretório existe."
html_title:           "Clojure: Verificando se um diretório existe."
simple_title:         "Verificando se um diretório existe."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Verificar se um diretório existe é uma tarefa comum em muitos projetos de desenvolvimento de software. Isso pode ser necessário para garantir que o diretório que você deseja acessar exista antes de realizar qualquer operação nele.

## Como fazer

Para verificar se um diretório existe, podemos usar a função `file-exists?` do pacote `clojure.java.io`. Esta função recebe uma string contendo o caminho absoluto ou relativo para o diretório e retorna `true` se o diretório existir ou `false` caso contrário.

```Clojure
;; Importar o pacote clojure.java.io
(require '[clojure.java.io :as io])

;; Verificar se o diretório "exemplo" existe no diretório atual
(file-exists? "exemplo")
;; => false

;; Criar o diretório "exemplo"
(doto (io/file "exemplo")
      (.mkdir))

;; Verificar novamente se o diretório existe
(file-exists? "exemplo")
;; => true
```

Se o diretório que você deseja verificar estiver em um caminho diferente do diretório atual, você pode usar a função `io/file-seq` para criar um objeto do tipo `File` e passá-lo como argumento para a função `file-exists?`.

```Clojure
;; Criar um objeto File para o diretório "exemplo"
(def exemplo-file (first (io/file-seq (io/file "/Users/username/Downloads/exemplo"))))

;; Verificar se o diretório existe
(file-exists? exemplo-file)
;; => true
```

Também é possível usar a função `ex-file` do pacote `clojure.java.io` para verificar se um diretório existe sem precisar importar o pacote.

```Clojure
(require '[clojure.java.io :refer [ex-file]])

;; Verificar se o diretório "exemplo" existe
(ex-file "exemplo")
;; => false
```

## Mergulho Profundo

Ao usar a função `file-exists?`, é importante entender que ela não verifica se o diretório está vazio ou se possui permissões de acesso corretas. Ela apenas verifica se o diretório especificado existe no sistema de arquivos.

Além disso, é importante notar que a função `file-exists?` também pode ser usada para verificar se um arquivo existe, já que um arquivo é apenas um tipo especial de diretório no sistema de arquivos.

## Veja também

- [Documentação oficial do pacote clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html) 
- [Artigo sobre como criar e manipular arquivos e diretórios em Clojure](https://www.advene.co/blog/file-and-directory-manipulation-in-clojure/)