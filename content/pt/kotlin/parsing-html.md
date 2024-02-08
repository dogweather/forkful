---
title:                "Análise de HTML"
aliases:
- pt/kotlin/parsing-html.md
date:                  2024-01-20T15:32:56.837266-07:00
simple_title:         "Análise de HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## O Que & Porquê?
Analisar (parsear) HTML é o processo de converter código HTML em uma estrutura de dados compreensível para a sua aplicação. Programadores fazem isso para manipular, extrair informações ou interagir com o conteúdo de sites.

## Como Fazer:
Você pode usar a biblioteca `jsoup` para parsear HTML em Kotlin. É simples e direto ao ponto. Aqui está um exemplo:

```kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Exemplo</title></head><body><p>Oi, mundo!</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    val bodyText = doc.body().text()

    println("Título da página: $title")
    println("Texto do corpo: $bodyText")
}
```

Saída de exemplo:
```
Título da página: Exemplo
Texto do corpo: Oi, mundo!
```

## Mergulho Profundo:
O parsing de HTML não é coisa nova, e tem evoluído junto com a web. No passado, muitas abordagens eram mais rudimentares e propensas a erros, como o uso de expressões regulares. Com o advento de bibliotecas robustas como `jsoup` em Java (e em Kotlin por extensão), foi simplificado não só o processo de parsing mas também a manipulação do DOM (Document Object Model) de uma forma segura.

Outras alternáculos incluem `HtmlUnit` e `Jaunt`, mas `jsoup` é conhecida por sua facilidade de uso e grande comunidade. Em termos de implementação, `jsoup` usa um analisador (parser) de HTML interno que entende as nuances de um HTML “real” encontrado na web, não apenas HTML bem-formado, tornando-o resistente e flexível para lidar com os diversos problemas que um HTML irregular pode apresentar.

## Veja Também:

- Documentação jsoup: [jsoup: Java HTML Parser](https://jsoup.org/)
- Repositório GitHub para jsoup: [GitHub - jsoup/jsoup: Java HTML Parser](https://github.com/jhy/jsoup)
- Guia sobre parsing de HTML em Kotlin usando jsoup: [Using jsoup in Kotlin](https://medium.com/@hadiyarajesh/using-jsoup-in-kotlin-99555bde6c94)
- Discussão no Stack Overflow sobre parsing de HTML com Kotlin: [Parsing HTML in Kotlin - Stack Overflow](https://stackoverflow.com/questions/4957597/how-to-parse-html-in-kotlin)
