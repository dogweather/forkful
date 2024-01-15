---
title:                "Baixando uma página da web"
html_title:           "Kotlin: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Por que

Existem várias razões pelas quais alguém pode querer fazer o download de uma página da web. Pode ser para ter acesso offline a um conteúdo importante, para arquivar informações ou para fins de análise de dados.

## Como fazer

Fazer o download de uma página da web em Kotlin é muito simples. Basta seguir os passos abaixo:

1. Primeiro, importe a biblioteca "kotlin.io" no seu projeto:
    ```Kotlin
    import kotlin.io.*
    ```

2. Em seguida, defina uma URL para a página que deseja fazer o download:
    ```Kotlin
    val url = "https://www.example.com"
    ```

3. Use o método "readText" da classe "URL" para fazer o download do conteúdo da página:
    ```Kotlin
    val pageContent = URL(url).readText()
    ```

4. E, por fim, salve o conteúdo em um arquivo utilizando o método "writeText" da classe "File":
    ```Kotlin
    File("filename.html").writeText(pageContent)
    ```

Ao executar esses passos, você terá feito o download da página da web em um arquivo HTML.

## Mergulho profundo

Existem várias classes e métodos dentro da biblioteca "kotlin.io" que podem ser úteis ao fazer o download de uma página da web. Alguns deles incluem:

- "readBytes()" - este método lê os bytes da página da web e os retorna como um array.
- "readLines()" - este método lê as linhas da página da web e as retorna como uma lista de strings.
- "URLStreamHandler" - esta classe permite a implementação de um manipulador personalizado para lidar com conexões de URL.

Além disso, existem outras bibliotecas disponíveis em Kotlin que podem ser úteis para aprofundar seus conhecimentos sobre o download de páginas da web, como a biblioteca "kotlinx.html" para lidar com documentos HTML de forma fácil e a biblioteca "kotlin-stdlib-js" para manipulação de dados em JavaScript.

## Veja também

- [Documentação oficial do Kotlin sobre download de URLs](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.net.-u-r-l/index.html)
- [Exemplo de download de página da web em Kotlin](https://github.com/kotlin-examples/internet/blob/master/src/download.kt)