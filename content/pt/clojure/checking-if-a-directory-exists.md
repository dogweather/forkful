---
title:    "Clojure: Verificando se um diretório existe"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Por que

Quando se está programando em Clojure, muitas vezes é necessário verificar se um diretório existe antes de executar certas ações. Isso pode ser feito para garantir que o programa não encontre erros ou para realizar operações específicas apenas se o diretório existir.

## Como Fazer

Para verificar se um diretório existe em Clojure, podemos usar a função `file-seq` e o operador `exists?`.

```Clojure
(def dir "~\downloads")
(if (exists? (file-seq dir)))
  (println "O diretório existe!")
  (println "O diretório não existe")
```

Neste exemplo, usamos a variável `dir` para armazenar o caminho do diretório que queremos verificar. Em seguida, usamos a função `file-seq` para criar uma sequência de arquivos dentro do diretório especificado. Finalmente, usamos o operador `exists?` para verificar se a sequência de arquivos existe ou não.

Se o diretório existir, a primeira mensagem será impressa. Caso contrário, a segunda mensagem será exibida.

## Mais Detalhes

Além do método mencionado acima, também podemos utilizar a biblioteca `clojure.java.io` para verificar a existência de um diretório. Isso pode ser feito usando a função `file` e a função `exists?`.

```Clojure
(require '[clojure.java.io :as io])

(def dir "~\downloads")
(if (exists? (io/file dir)))
  (println "O diretório existe!")
  (println "O diretório não existe")
```

Neste exemplo, usamos a função `file` para criar um objeto que representa o diretório especificado. Em seguida, usamos a função `exists?` para verificar se o diretório existe ou não.

## Veja Também

- Documentação oficial da função `exists?` em Clojure: https://clojuredocs.org/clojure.core/exists_q
- Tutorial sobre o uso da biblioteca `clojure.java.io`: https://practicalli.github.io/blog/18-clojure-java-io-library/