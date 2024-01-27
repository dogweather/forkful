---
title:                "Trabalhando com CSV"
date:                  2024-01-19
html_title:           "Bash: Trabalhando com CSV"
simple_title:         "Trabalhando com CSV"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/working-with-csv.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Trabalhar com CSV - Comma-Separated Values (Valores Separados por Vírgula) - significa lidar com arquivos de texto usados para armazenar dados. Programadores usam CSV por ser simples, amplamente suportado e fácil de importar ou exportar de planilhas e bancos de dados.

## How to:

```kotlin
// Utilizando a biblioteca kotlin-csv

import com.github.doyaaaaaken.kotlincsv.dsl.csvReader

fun main() {
    val csvContent = """
        nome,idade,profissão
        Joana,29,Desenvolvedora
        Carlos,35,Designer
    """.trimIndent()

    csvReader().readAllWithHeader(csvContent).forEach { row ->
        println("Nome: ${row["nome"]}, Idade: ${row["idade"]}, Profissão: ${row["profissão"]}")
    }
}
```

Exemplo de saída:

```
Nome: Joana, Idade: 29, Profissão: Desenvolvedora
Nome: Carlos, Idade: 35, Profissão: Designer
```

## Deep Dive:

CSV surgiu nos primeiros dias da computação pessoal e não tem um padrão rigoroso, resultando em variações. Alternativas modernas incluem JSON e XML, que são mais expressivos mas também mais verbosos. Ao lidar com CSV em Kotlin, considere usar bibliotecas como 'kotlin-csv' para abstrair complexidades como escape de caracteres e linhas quebradas.

## See Also:

- Especificações RFC 4180 para CSV: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
