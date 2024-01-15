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

## Por que

Se você trabalha com desenvolvimento web, é bem provável que em algum momento precise extrair informações de uma página HTML. E para isso, é necessário um processo chamado "parse", que consiste em analisar e extrair dados específicos de um código HTML. No Kotlin, existem diversas ferramentas que facilitam esse processo e tornam o parsing de HTML muito mais eficiente.

## Como fazer

Para realizar o parsing de HTML em Kotlin, primeiro é necessário adicionar a dependência "jsoup" ao seu projeto. Você pode fazer isso adicionando a seguinte linha no seu arquivo "build.gradle" na seção de dependências:

```
dependencies {
    implementation 'org.jsoup:jsoup:1.13.1'
}
```

Em seguida, é necessário importar a biblioteca no seu código Kotlin:

```
import org.jsoup.Jsoup
```

Agora, podemos utilizar o método `Jsoup.parse()` para realizar o parsing de um documento HTML. Por exemplo, se queremos fazer o parsing do conteúdo de uma URL, podemos fazer o seguinte:

```
val doc = Jsoup.parse(URL("https://www.example.com").readText())
```

O método `parse()` retorna um objeto da classe `Document` que nos permite utilizar diversos métodos para navegar e extrair informações do documento HTML. Por exemplo, podemos utilizar o método `select()` para selecionar elementos específicos do documento. 

```
// selecionando todos os elementos que possuem a tag "h1"
val elementosH1 = doc.select("h1")

//imprimindo o conteúdo do primeiro elemento selecionado
println(elementosH1.first().text())
```

O código acima irá imprimir o conteúdo do primeiro elemento `h1` encontrado no documento. Você também pode utilizar outros métodos, como `getElementById()` e `getElementsByClass()`, para selecionar elementos específicos do documento.

## Deep Dive

O jsoup é uma biblioteca bastante completa para realizar parsing de HTML em Kotlin, porém, é importante salientar que ela não é recomendada para casos mais complexos, como páginas dinâmicas que fazem uso de JavaScript. Nesses casos, é necessário utilizar ferramentas mais avançadas como o Selenium WebDriver. Além disso, é importante ter conhecimento de como funciona o DOM (Document Object Model) e as diferentes estruturas de uma página HTML.

## Veja também

- [Documentação oficial do jsoup](https://jsoup.org/cookbook/extracting-data/selector-syntax)
- [Tutorial de parsing de HTML em Kotlin com jsoup](https://kotlinexpertise.com/parse-html-kotlin-jsoup/)
- [Documentação oficial do Selenium WebDriver](https://www.selenium.dev/documentation/en/webdriver)