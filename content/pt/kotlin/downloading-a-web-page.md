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

## O que é isso e por que fazer?

Baixar uma página da web é o processo de fazer o download de um arquivo HTML da internet. Isso é feito por programadores para acessar e usar o conteúdo de uma página da web em seu próprio aplicativo ou software. Isso pode ser útil para criar web scrapers, feito para extrair dados específicos de um site para uso em outras aplicações.

## Como fazer:

```Kotlin
val url = URL("https://example.com")
val conexao = url.openConnection() as HttpURLConnection
conexao.requestMethod = "GET"
conexao.connect()

val resposta = conexao.inputStream.bufferedReader().use { it.readText() }
println(resposta)
```

**Output:**
```
<!DOCTYPE html>
<html>
    <head>
        <title>Example Domain</title>

        <meta charset="utf-8" />
        <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <style type="text/css">
        body {
            background-color: #f0f0f2;
            margin: 0;
            padding: 0;
            font-family: "Open Sans", "Helvetica Neue", Helvetica, Arial, sans-serif;
            
        ...
```

## Detalhes mais profundos:

Fazer o download de uma página da web é uma tarefa comum e essencial para muitos programadores. Existem diversas ferramentas e bibliotecas disponíveis para fazer isso em diferentes linguagens de programação, incluindo Kotlin. Além disso, existem várias técnicas e abordagens para baixar o conteúdo de uma página da web, dependendo das necessidades do projeto. 

## Veja também:

- Documentação do Kotlin sobre classes URL e HttpURLConnection: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.net/java.net.-u-r-l/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.net/java.net.-u-r-l/)
- Tutorial sobre web scraping com Kotlin: [https://medium.com/kotlin-th/kotlin-for-android-tutorial-lesson-7-web-scraping-with-kotlin-efdb7fe3af68](https://medium.com/kotlin-th/kotlin-for-android-tutorial-lesson-7-web-scraping-with-kotlin-efdb7fe3af68)
- Alternativas para baixar conteúdo da web em Kotlin: [https://github.com/KotlinBy/awesome-kotlin#networking](https://github.com/KotlinBy/awesome-kotlin#networking)