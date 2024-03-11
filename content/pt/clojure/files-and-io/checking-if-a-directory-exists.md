---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:46.318612-07:00
description: "Verificar se um diret\xF3rio existe em Clojure envolve verificar a presen\xE7\
  a de um diret\xF3rio no sistema de arquivos a partir de sua aplica\xE7\xE3o Clojure.\
  \ Esta\u2026"
lastmod: '2024-03-11T00:14:19.885179-06:00'
model: gpt-4-0125-preview
summary: "Verificar se um diret\xF3rio existe em Clojure envolve verificar a presen\xE7\
  a de um diret\xF3rio no sistema de arquivos a partir de sua aplica\xE7\xE3o Clojure.\
  \ Esta\u2026"
title: "Verificando se um diret\xF3rio existe"
---

{{< edit_this_page >}}

## O Que & Por Quê?
Verificar se um diretório existe em Clojure envolve verificar a presença de um diretório no sistema de arquivos a partir de sua aplicação Clojure. Esta tarefa é crucial para operações com arquivos, para prevenir erros ao ler ou escrever em diretórios que podem não estar presentes, garantindo execução de código robusta e livre de erros.

## Como fazer:
Clojure, sendo uma linguagem JVM, pode utilizar a classe `java.io.File` do Java para esse propósito. Você não precisa de nenhuma biblioteca de terceiros para uma operação tão básica. Veja como você pode fazer isso:

```clojure
(import 'java.io.File)

(defn directory-exists? [dir-path]
  (let [dir (File. dir-path)]
    (.exists dir)))

;; Exemplo de uso
(println (directory-exists? "/caminho/para/seu/diretorio")) ;; verdadeiro ou falso
```

Esta função, `directory-exists?`, recebe um caminho de diretório como uma string e retorna `true` se o diretório existe e `false` caso contrário. Isso é alcançado criando um objeto `File` com o caminho do diretório e então chamando o método `.exists` nesse objeto.

Além da interoperabilidade com Java puro, você pode usar bibliotecas Clojure que abstraem parte do boilerplate Java. Uma dessas bibliotecas é `clojure.java.io`. No entanto, para verificar se um diretório existe, você ainda usaria a classe `File`, mas poderá achar a biblioteca útil para outras operações com arquivos. Exemplo:

```clojure
(require '[clojure.java.io :as io])

(defn directory-exists?-clojure [dir-path]
  (.exists (io/file dir-path)))

;; Exemplo de uso
(println (directory-exists?-clojure "/outro/caminho/para/verificar")) ;; verdadeiro ou falso
```

Esta versão é bastante semelhante, mas usa a função `io/file` do Clojure para criar o objeto `File`. Este método se integra de maneira mais natural a bases de código Clojure ao aproveitar a biblioteca de Clojure para operações de IO, em vez de interagir diretamente com classes Java.
