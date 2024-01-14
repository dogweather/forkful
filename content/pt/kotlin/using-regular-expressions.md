---
title:                "Kotlin: Utilizando expressões regulares"
simple_title:         "Utilizando expressões regulares"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Por que utilizar expressões regulares em Kotlin?

As expressões regulares são uma ferramenta poderosa para buscar e manipular texto em linguagens de programação. Em Kotlin, elas permitem que os desenvolvedores realizem tarefas de forma mais eficiente e com menos linhas de código. Além disso, a utilização de expressões regulares pode ser extremamente útil para tarefas de validação e filtragem de dados.

## Como utilizar expressões regulares em Kotlin?

A sintaxe para utilizar expressões regulares em Kotlin é semelhante a outras linguagens de programação, como Java. Primeiro, é necessário importar a classe correspondente ao uso de expressões regulares:

```Kotlin
import kotlin.text.Regex
```

Com a classe importada, podemos criar um objeto Regex para representar nossa expressão regular, utilizando o construtor que recebe como parâmetro uma string com a expressão. Por exemplo, para encontrar todas as ocorrências de um determinado padrão de caracteres (nesse caso, números seguidos de letras maiúsculas) em uma string, podemos utilizar o seguinte código:

```Kotlin
val pattern = "[0-9]+[A-Z]+".toRegex()
val result = pattern.findAll("12345ABCDE").toList()
println(result) // saída: [12345, ABCDE]
```

Podemos utilizar diversas funções da classe Regex para aplicar essa expressão em diferentes situações e obter resultados específicos, como substituir uma parte de uma string ou verificar se uma string segue um determinado padrão.

## Aprofundando no uso de expressões regulares

Além das funcionalidades básicas, as expressões regulares em Kotlin possuem algumas particularidades interessantes. Por exemplo, é possível utilizar grupos de captura para acessar partes específicas da string que foi encontrada pelo padrão, utilizando a propriedade `groups`. Também é possível utilizar classes de caractere específicas, como `\d` para representar qualquer dígito ou `\w` para representar qualquer caractere alfanumérico.

Outra funcionalidade útil é a possibilidade de utilizar o modo multiline, que permite que a expressão regular seja aplicada em várias linhas de uma string. Isso pode ser especialmente útil para processar textos com quebras de linha.

Se você deseja aprender mais sobre expressões regulares em Kotlin, pode consultar a documentação oficial da classe Regex e também alguns tutoriais e exercícios disponíveis online, como os do site *RegexOne* (https://regexone.com/).

## Veja também

- Documentação oficial da classe Regex em Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- Tutorial do site RegexOne: https://regexone.com/
- Exemplos práticos de uso de expressões regulares em Kotlin: https://www.javatpoint.com/kotlin-regular-expression