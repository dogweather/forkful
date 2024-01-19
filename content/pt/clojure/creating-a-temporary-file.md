---
title:                "Criando um arquivo temporário"
html_title:           "Bash: Criando um arquivo temporário"
simple_title:         "Criando um arquivo temporário"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## O Que é & Porquê?

Criar um arquivo temporário significa gerar um arquivo para armazenamento temporário de dados, que é útil em operações de I/O (input/output). Programadores fazem isso para salvar dados intermediários, resolver problemas de conversão e lidar com grandes volumes de informação que não cabem na memória.

## Como Faz:

Clojure, sendo uma linguagem moderna e concisa, tem a biblioteca java.nio.file que facilita a criação de arquivos temporários.
```clojure
(import 'java.nio.file.Files)
(import 'java.nio.file.Paths)

(defn create-temp-file []
  (Files/createTempFile (Paths/get "/tmp" (into-array String [])) "temp" ".txt"))
```
Este código define uma função chamada `create-temp-file`, que cria um arquivo temporário chamado "temp.txt". Você pode ver a saída abaixo:
```clojure
#object[java.nio.file.Path 0x6e38ffe0 "/tmp/temp5814890766880106554.txt"]
```
Este é o caminho para o arquivo temporário criado.

## Aprofundando:

Historicamente, antes dos pacotes java.nio.file, a criação de arquivos temporários era mais engorçada, muitas vezes envolvendo manipulação manual de strings para caminhos de arquivo.

Alternativamente, você pode usar a função de arquivo temporário no pacote java.io:
```clojure
(import 'java.io.File)
(defn create-temp-file []
  (.createTempFile (File. "/tmp") "temp" ".txt"))
```
Mas java.nio.file é recomendado por ser mais moderno e versátil.

Relativamente a detalhes de implementação, os arquivos temporários são armazenados no diretório especificado (aqui, "/tmp"). O nome do arquivo é uma combinação do prefixo fornecido ("temp") e um longo gerado automaticamente para garantir unicidade.

## Veja Também:

- [Clojure Java Interop](https://clojure.org/reference/java_interop)
- [Java Doc - Files](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html)
- [Java Doc - Paths](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Paths.html)