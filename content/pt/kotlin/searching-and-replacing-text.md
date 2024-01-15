---
title:                "Buscando e substituindo texto"
html_title:           "Kotlin: Buscando e substituindo texto"
simple_title:         "Buscando e substituindo texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Por que

À medida que os desenvolvedores avançam em suas carreiras e ganham mais experiência, eles geralmente são expostos a diferentes linguagens de programação. Mudar de uma linguagem para outra pode ser intimidante, mas sabendo como usar recursos básicos, como a busca e substituição de texto, pode ajudar a facilitar a transição.

## Como fazer

Para realizar uma busca e substituição de texto em Kotlin, siga os seguintes passos:

1. Declare uma variável do tipo `String` com o texto que será modificado: 

   ```Kotlin
   val texto = "Estou aprendendo Kotlin!"
   ```

2. Use o método `replace()` para substituir o texto desejado por outro:

   ```Kotlin
   val novoTexto = texto.replace("Kotlin", "básico de Kotlin")
   ```

3. Imprima o resultado usando o método `println()`:

   ```Kotlin
   println(novoTexto) // Saída: "Estou aprendendo básico de Kotlin!"
   ```

## Deep Dive

Além do exemplo básico acima, Kotlin oferece recursos adicionais para tornar a busca e substituição de texto ainda mais poderosa. Aqui estão alguns deles:

- O método `replaceFirst()` pode ser usado para substituir apenas a primeira ocorrência do texto desejado. Por exemplo:

  ```Kotlin
  val texto = "Kotlin é uma linguagem de programação poderosa e moderna"
  val novoTexto = texto.replaceFirst("poderosa", "incrível")
  println(novoTexto) // Saída: "Kotlin é uma linguagem de programação incrível e moderna"
  ```

- É possível usar expressões regulares no lugar do texto a ser substituído. Isso torna a busca mais flexível e abrangente. Por exemplo:

  ```Kotlin
  val texto = "Estou escrevendo em camelCase"
  val novoTexto = texto.replace("[A-Z]".toRegex(), "-")
  println(novoTexto) // Saída: "-stou escrevendo em camel-ase"
  ```

  Neste exemplo, as letras maiúsculas são substituídas por hífens, criando uma separação entre as palavras do texto.

- Também é possível usar um bloco de códigos como parâmetro para o método `replace()`. Isso permite que você personalize ainda mais a substituição de texto. Por exemplo:

  ```Kotlin
  val texto = "Aprendendo Kotlin é divertido"
  val novoTexto = texto.replace("Kotlin") {
      it.value.toUpperCase()
  }
  println(novoTexto) // Saída: "Aprendendo KOTLIN é divertido"
  ```

  Neste exemplo, a palavra "Kotlin" é substituída pelo seu valor em letras maiúsculas.

## Veja também

- [Documentação oficial do Kotlin sobre busca e substituição de texto](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-replace.html)
- [Tutorial de Kotlin do site Codecademy](https://www.codecademy.com/learn/learn-kotlin/modules/learn-kotlin-loops-and-string-templates/cheatsheet)
- [Vídeo do YouTube sobre busca e substituição de texto em Kotlin](https://www.youtube.com/watch?v=szLRXUjPNlQ)