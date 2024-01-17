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

## O que & Por quê?

Verificar se um diretório existe é uma tarefa comum para programadores em Clojure. Isso significa checar se um determinado diretório ou pasta está presente em uma localização específica no sistema de arquivos. É importante fazer essa verificação para garantir que o diretório necessário esteja disponível antes de executar certas operações, como criar um novo arquivo ou copiar um arquivo para ele.

## Como fazer:

```Clojure
(require '[clojure.java.io :as io])

(if (io/file-exists? "caminho/para/diretorio")
  (println "O diretório existe!")
  (println "O diretório não existe!")
)
```

Saída:

```
O diretório existe!
```

Primeiro, importamos a biblioteca "clojure.java.io" para poder usar suas funções relacionadas a entrada e saída. Em seguida, usamos a função "file-exists?" para verificar se o caminho especificado leva a um diretório existente. Se o diretório existir, a saída será "O diretório existe!", caso contrário, a saída será "O diretório não existe!".

## Mergulho profundo:

### Contexto histórico:

A capacidade de verificar se um diretório existe foi introduzida na linguagem Clojure no ano de 2010, com a versão 1.2.

### Alternativas:

Uma alternativa para verificar se um diretório existe é usar a função "exists?" da biblioteca "clojure.contrib.io", que também possui uma função "file?" que pode ser usada para verificar se o caminho especificado leva a um diretório ou arquivo.

### Detalhes de implementação:

No nível do sistema operacional, a verificação de existência de diretório usa a função "stat", que retorna informações sobre um arquivo ou diretório, incluindo se ele existe ou não.

## Veja também:

- [Documentação da função "io/file-exists?"](https://clojuredocs.org/clojure.java.io/file-exists_q)
- [Mais informações sobre a biblioteca "clojure.java.io"](https://clojuredocs.org/clojure.java.io)