---
title:                "Trabalhando com csv"
html_title:           "Kotlin: Trabalhando com csv"
simple_title:         "Trabalhando com csv"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## Por que trabalhar com CSV?

CSV (Comma-Separated Values) é um formato de arquivo amplamente usado para armazenar dados tabulares. Ao trabalhar com CSV, é possível importar e exportar dados de e para uma variedade de fontes, tornando-o extremamente útil em casos de uso, como análise de dados, integrações de sistemas e geração de relatórios.

## Como fazer?

Usando a linguagem de programação Kotlin, é simples trabalhar com CSV. Primeiro, precisamos importar a biblioteca ``okhttp`` e definir o URL onde o arquivo CSV está armazenado. Em seguida, podemos fazer uma solicitação GET para obter o conteúdo do CSV e, finalmente, converter os dados para uma lista de listas contendo todas as linhas e colunas do arquivo.

```
// Importando a biblioteca okhttp
import okhttp3.OkHttpClient
import okhttp3.Request

// Definindo o URL do arquivo CSV
val url = "https://exemplo.com/arquivo.csv" 

// Fazendo uma solicitação GET para obter o conteúdo do CSV
val client = OkHttpClient()
val request = Request.Builder().url(url).build()
val response = client.newCall(request).execute()

// Convertendo os dados para uma lista de listas
val csvData = response.body()?.string()?.split("\n")?.map { it.split(",") }
```

Agora podemos facilmente acessar os dados do CSV, por exemplo, imprimindo a primeira linha e coluna:

```
println(csvData[0][0]) // Imprime o primeiro valor do arquivo CSV
```

## Mergulho profundo

Existem várias bibliotecas e ferramentas disponíveis para trabalhar com CSV em Kotlin, como a ``okhttp`` que usamos no exemplo acima. Além disso, é possível usar a biblioteca padrão ``java.io.File`` para ler e escrever em arquivos CSV locais. Além disso, o Kotlin possui recursos avançados, como a função ``useLines`` que facilita a leitura de grandes arquivos CSV sem a necessidade de carregar todos os dados na memória.

## Veja também

- [Documentação oficial do Kotlin](https://kotlinlang.org/)
- [Tutorial: Como trabalhar com CSV em Kotlin usando a biblioteca Apache Commons](https://www.jetbrains.com/help/idea/working-with-csv-and-tab-delimited-files.html)
- [Código fonte do exemplo](https://github.com/example/csv-kotlin-example)