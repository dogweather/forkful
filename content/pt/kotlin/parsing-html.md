---
title:                "Analisando html"
html_title:           "Kotlin: Analisando html"
simple_title:         "Analisando html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## O que é e por que fazer?
HTML é a linguagem padrão para criação de páginas na internet. Entretanto, muitas vezes é necessário acessar e manipular esse código de forma automatizada. É aí que entra o parsing HTML. Parsing HTML é o processo de analisar o código HTML para extrair e usar as informações nele contidas. Programadores utilizam essa técnica para tarefas como web scraping, onde é necessário coletar dados de diferentes páginas.

## Como fazer:
Usando Kotlin, é possível realizar parsing HTML de forma simples e eficiente. Segue abaixo um exemplo de código e a saída correspondente para a página "https://www.google.com.br/":

```Kotlin
val url = "https://www.google.com.br/"
val conn = URL(url).openConnection() as HttpURLConnection
val html = BufferedReader(InputStreamReader(conn.inputStream)).use { it.readText() }
val doc = Jsoup.parse(html)
println(doc.title()) // output: "Google"
```

## Mergulho profundo:
Parsing HTML não é uma técnica nova. Desde os primórdios da internet, já havia a necessidade de manipular o código para fins específicos. Além de Kotlin, existem outras linguagens que possuem bibliotecas dedicadas a realizar essa tarefa, como por exemplo o Python com a biblioteca BeautifulSoup. Em termos de implementação, o processo de parsing envolve principalmente a utilização de expressões regulares para identificar e extrair as informações desejadas.

## Veja também:
- Documentação do Jsoup: https://jsoup.org/
- Tutorial sobre parsing HTML com Kotlin: https://www.tutorialspoint.com/jsoup/jsoup_parsing_html.htm
- Biblioteca BeautifulSoup para Python: https://www.crummy.com/software/BeautifulSoup/bs4/doc/