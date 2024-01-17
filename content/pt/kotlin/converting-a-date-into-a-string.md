---
title:                "Convertendo uma data em uma string"
html_title:           "Kotlin: Convertendo uma data em uma string"
simple_title:         "Convertendo uma data em uma string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## O que é isso e por quê?

Converter uma data para uma string é um processo comum na programação, onde uma data é transformada em um formato de texto legível para ser mostrado ou armazenado. Isso é útil para exibir informações de data em interfaces de usuário ou para armazenamento em bancos de dados. Programadores fazem isso para tornar as informações de data mais acessíveis e utilizáveis em seus códigos.

## Como fazer:

```
Kotlin val data = Date() // cria uma nova data
val formatter = SimpleDateFormat("dd.MM.yyyy") // define o formato da string
val dataString = formatter.format(data) // converte a data para string
println(dataString) // imprime a data em formato de texto (ex: 13.09.2021)
```

## Mergulho profundo:

Históricamente, o processo de converter uma data para uma string era feito de forma manual, utilizando fórmulas matemáticas complexas. No entanto, com o avanço da tecnologia, linguagens de programação oferecem métodos automatizados para essa tarefa, o que torna o processo muito mais simples e rápido.

Uma alternativa para converter uma data em uma string é utilizar bibliotecas de terceiros, como o Joda-Time ou o java.time, que possuem funções e métodos mais avançados para manipulação de datas.

Na implementação do Kotlin, existe o pacote "java.text" que contém as classes SimpleDateFormat e DateFormat, que podem ser utilizadas para formatar e converter datas em strings. Além disso, o Kotlin também possui uma classe nativa chamada "DateTimeFormatter" que facilita o processo de conversão de uma data para uma string com métodos mais intuitivos e flexíveis.

## Veja também:

- [Documentação oficial do Kotlin sobre formatação de datas](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/java.text.-date-format/)
- [Joda-Time API](https://www.joda.org/joda-time/)
- [Java Date and Time API](https://docs.oracle.com/javase/tutorial/datetime/index.html)