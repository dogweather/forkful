---
title:    "Clojure: Escrevendo um arquivo de texto"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Por que escrever um arquivo de texto em Clojure?

Escrever um arquivo de texto é uma tarefa comum em muitas linguagens de programação, incluindo Clojure. Essa prática pode ser útil para armazenar dados, como resultados de um programa, ou para criar arquivos de configuração que possam ser facilmente lidos e modificados.

## Como fazer?

Para escrever um arquivo de texto em Clojure, primeiro precisamos criar um objeto "java.io.FileWriter" e passar o caminho e o nome do arquivo que desejamos criar. Em seguida, podemos usar o comando "write" para adicionar conteúdo ao arquivo. Aqui está um exemplo de código em Clojure que escreve "Hello World!" em um arquivo chamado "exemplo.txt":

```Clojure
(with-open [fw (java.io.FileWriter. "exemplo.txt")]
  (.write fw "Hello World!"))
```

Podemos verificar se o arquivo foi criado com sucesso usando o comando "ls" no terminal. A saída deve incluir o arquivo "exemplo.txt". Quando abrimos o arquivo, devemos ver a mensagem "Hello World!" dentro dele.

## Mergulho Profundo

Ao escrever um arquivo de texto em Clojure, é importante lembrar de fechar o objeto "FileWriter" após terminar de escrever. Podemos fazer isso usando o comando "close" ou usando "with-open" como no exemplo anterior.

Também é possível usar outros métodos, como "append" para adicionar conteúdo a um arquivo já existente ou "println" para escrever uma nova linha. Além disso, podemos usar o comando "print-str" para escrever conteúdo semelhante a um objeto "String" diretamente no arquivo.

## Veja também

Aqui estão alguns links úteis para aprender mais sobre como escrever arquivos de texto em Clojure:

- [Documentação oficial de FileWriter](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Exemplo de escrita de arquivo em Clojure](https://www.baeldung.com/clojure-writing-text-files)
- [Tutorial em vídeo sobre como escrever arquivos em Clojure](https://www.youtube.com/watch?v=aBaAa4fCrPc)