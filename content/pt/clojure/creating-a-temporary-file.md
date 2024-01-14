---
title:    "Clojure: Criando um arquivo temporário"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Por que criar um arquivo temporário em Clojure?

Neste artigo, vamos explorar a utilidade e a importância de criar arquivos temporários em um programa feito em Clojure. Veremos como isso pode facilitar o gerenciamento de arquivos e a organização do código. 

## Como criar um arquivo temporário em Clojure

Para criar um arquivo temporário em Clojure, podemos utilizar a função `java.io.File/createTempFile`, que recebe dois parâmetros: o prefixo do nome do arquivo e o sufixo. Veja um exemplo abaixo:

```Clojure
(import java.io.File)

(def temporary-file (File/createTempFile "exemplo-" ".txt"))
```

Ao executar esse código, será criado um arquivo temporário com o nome `exemplo-#####.txt`, onde `####` é um número gerado automaticamente pelo sistema operacional. Agora, podemos escrever no arquivo utilizando a função `clojure.java.io/copy`, como mostrado abaixo:

```Clojure
(require '[clojure.java.io :as io])

(io/copy "Olá, mundo!" temporary-file)
```

O conteúdo "Olá, mundo!" será copiado para o arquivo temporário. Para ler o conteúdo do arquivo, podemos utilizar a função `slurp`:

```Clojure
(slurp temporary-file)
;; saída: "Olá, mundo!"
```

Podemos também especificar um diretório onde o arquivo temporário deve ser criado usando o parâmetro `:directory` na função `createTempFile`. Por exemplo:

```Clojure
(def dir (File. "caminho/para/pasta"))
(def temporary-file (File/createTempFile "exemplo-" ".txt" :directory dir))
```

## Aprofundando-se na criação de arquivos temporários

Os arquivos temporários são úteis em várias situações, como salvar dados temporários, gerar arquivos para processamentos específicos ou até mesmo fazer testes rápidos sem precisar criar um arquivo permanente. Além disso, eles são automaticamente excluídos pelo sistema operacional após o seu uso, evitando a sobrecarga de arquivos desnecessários no sistema. 

Existem também outras opções de criação de arquivos temporários em Clojure, como a função `java.io.File/createTempFile` que permite especificar um prefixo, sufixo e um diretório para a criação do arquivo, e a função `java.nio.file.Files/createTempFile` que retorna um objeto `Path` em vez de um `File`.

Não se esqueça de sempre excluir o arquivo temporário após o uso, utilizando a função `java.io.File/delete`, para evitar a sobrecarga de arquivos no sistema.

## Veja também

- Documentação oficial do `java.io.File`: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html
- Documentação oficial do `java.nio.file.Files`: https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html
- Tutorial sobre manipulação de arquivos em Clojure: https://clojure.org/guides/io