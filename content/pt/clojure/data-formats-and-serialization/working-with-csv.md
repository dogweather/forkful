---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:09.728200-07:00
description: 'Como fazer: #.'
lastmod: '2024-03-13T22:44:46.218931-06:00'
model: gpt-4-0125-preview
summary: '#.'
title: Trabalhando com CSV
weight: 37
---

## Como fazer:


### Lendo um arquivo CSV
Clojure não tem análise de CSV embutida em sua biblioteca padrão, mas você pode usar a biblioteca `clojure.data.csv` para esse propósito. Primeiro, adicione a biblioteca às dependências do seu projeto.

No seu `project.clj`, adicione a seguinte dependência:
```clojure
[clojure.data.csv "1.0.0"]
```
Para ler um arquivo CSV e imprimir cada linha:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(with-open [reader (io/reader "caminho/para/seuarquivo.csv")]
  (doall
   (map println (csv/read-csv reader))))
```
Isso irá exibir cada linha do CSV como um vetor Clojure.

### Escrevendo em um arquivo CSV
Para escrever dados em um arquivo CSV, você pode usar a mesma biblioteca `clojure.data.csv`:
```clojure
(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(let [data [["id" "nome" "idade"]
            ["1" "John Doe" "28"]
            ["2" "Jane Doe" "31"]]]
  (with-open [writer (io/writer "caminho/para/arquivosaida.csv")]
    (csv/write-csv writer data)))
```
Isso cria ou sobrescreve `arquivosaida.csv`, preenchendo-o com os dados especificados.

### Usando uma Biblioteca de Terceiros: `clojure.data.csv`
Embora `clojure.data.csv` seja, sem dúvida, a biblioteca mais direta para o manuseio de CSV em Clojure, para tarefas mais complexas, como lidar com CSVs com caracteres especiais ou delimitadores não convencionais, você pode explorar opções adicionais dentro do ecossistema ou até mesmo considerar a interoperabilidade com Java com bibliotecas como Apache Commons CSV. No entanto, para a maioria das tarefas de processamento de CSV padrão em Clojure, `clojure.data.csv` fornece um conjunto de ferramentas simples e eficaz.
