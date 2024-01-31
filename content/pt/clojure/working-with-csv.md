---
title:                "Trabalhando com CSV"
date:                  2024-01-19
simple_title:         "Trabalhando com CSV"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/clojure/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que & Por Que?

Trabalhar com CSV (valores separados por vírgula) é manejar dados em formato de texto, bastante usado devido à simplicidade e compatibilidade com várias ferramentas, como planilhas e bancos de dados. Programadores usam CSV para importar, exportar, e manipular dados de maneira fácil e rápida entre diferentes sistemas.

## Como Fazer:

```Clojure
; Importar a biblioteca clojure.data.csv 
(require '[clojure.data.csv :as csv])
(require '[clojure.java.io :as io])

; Ler dados de um arquivo CSV e transformá-los em vetores
(with-open [reader (io/reader "dados.csv")]
  (let [data (csv/read-csv reader)]
    (println data)))

; Escrever dados em um arquivo CSV
(let [data [["nome", "idade", "cidade"]
            ["Ana", "28", "Lisboa"]
            ["João", "35", "Porto"]]
      writer (io/writer "saida.csv")]
  (csv/write-csv writer data))
```
Saída exemplo ao ler `"dados.csv"`:

```
[["nome" "idade" "cidade"] ["Ana" "28" "Lisboa"] ["João" "35" "Porto"]]
```

## Mergulho Profundo

CSV surgiu da necessidade de transferência de dados entre programas que não compartilhavam a mesma estrutura de armazenamento. É uma alternativa a formatos mais complexos como XML e JSON. Em Clojure, trabalhar com CSV é geralmente feito usando a biblioteca `clojure.data.csv`. Esta biblioteca é poderosa mas minimalista, e pode ser combinada com outras (como `clojure.java.io`) para leitura e escrita de arquivos.

## Veja Também

- Documentação oficial `clojure.data.csv`: [https://github.com/clojure/data.csv](https://github.com/clojure/data.csv)
- Guia de Clojure para IO: [https://clojure.org/guides/learn/functions#_file_io](https://clojure.org/guides/learn/functions#_file_io)
- Especificações do formato CSV pelo RFC 4180: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
