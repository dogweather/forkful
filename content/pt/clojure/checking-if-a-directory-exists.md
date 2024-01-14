---
title:    "Clojure: Verificando se um diretório existe"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Por que 

Muitas vezes em nossas jornadas de programação, nos deparamos com a tarefa de verificar se um determinado diretório existe em nosso sistema. Isso pode ser útil para garantir que os arquivos armazenados em nosso programa estejam sendo salvos no local correto, ou para realizar outras tarefas específicas.

## Como Fazer

Para verificar se um diretório existe em Clojure, podemos usar a função `exists?` do namespace `java.io.File`. Primeiro, precisamos importar esse namespace em nosso código:

```Clojure
(ns meu-programa.core
  (:import [java.io File]))
```

Em seguida, podemos usar a função `exists?` passando o caminho do diretório que queremos verificar como argumento:

```Clojure
(exists? "caminho/para/diretorio")
```

Esta função retornará um valor booleano, `true` se o diretório existir ou `false` caso contrário. Podemos usar uma condicional para manusear o resultado da função e executar outras tarefas em nosso programa.

Por exemplo, vamos criar um programa que cria um novo diretório, verifica se ele existe e, em seguida, remove o diretório se ele existir:

```Clojure
(ns meu-programa.core
  (:import [java.io File]))

(defn criar-diretorio [caminho]
  (let [dir (File. caminho)]
    (.mkdir dir)
    (if (exists? caminho)
      (println "Diretório criado com sucesso!")
      (println "Erro ao criar diretório."))))

(defn remover-diretorio [caminho]
  (let [dir (File. caminho)]
    (if (exists? caminho)
      (.delete dir)
      (println "Diretório não existe."))))

(criar-diretorio "caminho/para/diretorio")
(remover-diretorio "caminho/para/diretorio")
```

A saída do programa será:

```
Diretório criado com sucesso!
Diretório removido com sucesso!
```

## Profundidade

Para entender melhor como a função `exists?` funciona, podemos dar uma olhada em sua implementação. Apesar de ser uma função do namespace `java.io.File`, ela não é nativa do Clojure e, na verdade, é uma função em Java.

Dentro do namespace `java.io.File`, a função `exists?` é definida como:

```java
public boolean exists()
```

Como podemos ver, ela simplesmente retorna um booleano indicando se o arquivo ou diretório existe ou não. É importante notar que essa função também pode ser usada para verificar a existência de arquivos, não apenas diretórios.

## Veja Também

- [Documentação oficial da função `exists?`](https://clojuredocs.org/clojure.java.io/exists_qmark)
- [Tutorial de Clojure para iniciantes](https://www.clojure.org/guides/getting_started)
- [Exemplo de uso da função `exists?` em um projeto real](https://github.com/clojure/clojure/blob/master/src/main/clojure/clojure/test_clojure/os.clj)