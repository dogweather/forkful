---
title:                "Lidando com csv"
html_title:           "Kotlin: Lidando com csv"
simple_title:         "Lidando com csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## O que é CSV e por que os programadores o utilizam?
CSV é a sigla para Comma-Separated Values, uma forma de armazenar dados em formato tabular. É comumente utilizada para importar e exportar dados entre diferentes softwares, sendo bastante útil para o processamento de grandes quantidades de informações. Os programadores utilizam CSV para facilitar a troca de dados entre diferentes sistemas e para analisar e manipular dados de forma eficiente.

## Como fazer:
Na linguagem Kotlin, trabalhar com CSV é bastante simples. Primeiramente, é necessário importar a biblioteca "CsvReader" utilizando o comando ```import com.opencsv.CsvReader```. Em seguida, é possível ler um arquivo CSV utilizando o seguinte código:
```
val reader = CsvReader("arquivo.csv")
reader.readHeaders() // lê o cabeçalho do arquivo
while(reader.readRecord()){ // lê cada registro do arquivo
  val coluna1 = reader.get("coluna1") // acessa a coluna desejada para cada registro
  val coluna2 = reader.get("coluna2")
  println("$coluna1 - $coluna2") // imprime os dados da coluna desejada para cada registro
}
```
O resultado será a impressão dos dados da coluna1 e coluna2 do arquivo CSV. Além disso, também é possível utilizar a biblioteca "CsvWriter" para escrever em um arquivo CSV. A documentação oficial da biblioteca pode ser consultada para informações mais detalhadas.

## Aprofundando:
O CSV foi criado nos anos 1970 para facilitar a troca de dados entre diferentes programas, pois na época não existia uma padronização para o formato de arquivos de dados. Atualmente, existem diversos formatos de arquivos que podem ser utilizados para armazenar dados, como JSON e XML. No entanto, o CSV ainda é amplamente utilizado por ser simples e compatível com a maioria dos softwares.

## Veja também:
- Documentação oficial da biblioteca CsvReader: https://javadoc.io/doc/com.opencsv/opencsv/
- Tutorial sobre utilização da biblioteca CsvReader em Kotlin: https://www.callicoder.com/kotlin-read-write-csv-file/