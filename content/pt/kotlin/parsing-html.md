---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

A análise de HTML, ou parsing, é o processo de extrair dados a partir de documentos HTML. Programadores fazem isso para coletar e manipular informações específicas de páginas da web.

## Como fazer:

Aqui está uma amostra do código em Kotlin para parsear HTML utilizando a biblioteca jsoup:

```Kotlin
import org.jsoup.Jsoup
import org.jsoup.nodes.Document

fun parseHTML(html: String): Document {
    val doc: Document = Jsoup.parse(html)
    return doc
}
```
Em seguida, você pode facilmente extrair detalhes de elementos específicos:

```Kotlin
fun getElements(doc: Document, tagName: String) {
    val elements = doc.getElementsByTag(tagName)
    elements.forEach { element ->
        println(element.text())
    }
}
```
Executando o exemplo acima, você verá algo assim:

```Kotlin
heading1
heading2
heading3
```
## Mergulho Profundo:

Muitas bibliotecas de análise de HTML têm suas raízes no início da web, quando os documentos HTML não eram tão padronizados como são hoje. Antes da jsoup, muitas das bibliotecas disponíveis eram difíceis de usar e incompletas.

Existem alternativas à biblioteca jsoup, como o HtmlCleaner e o Jericho.

A implementação básica do parsing de HTML em Kotlin envolve o uso de uma biblioteca externa para fazer o heavy lifting. No caso de jsoup, a biblioteca faz o trabalho de construir uma árvore a partir do HTML, depois você pode simplesmente consultar essa árvore para extrair as informações que precisa.

## Veja Também:

1. [Jsoup - Library de Parsing em Java](https://jsoup.org/)
2. [HtmlCleaner - Java HTML Parser](https://htmlcleaner.sourceforge.io/)
3. [Jericho HTML Parser](https://jericho.htmlparser.net/docs/index.html)
4. [Análise de HTML no Android](https://developer.android.com/guide/topics/ui/declaring-layout)
5. [Parseando HTML em Kotlin com Jsoup](https://www.baeldung.com/kotlin-jsoup)