---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Descarregar uma página web é basicamente obter todo o conteúdo de uma página web específica para utilização local. Programadores geralmente fazem isso para manipular ou extrair dados dessas páginas para vários propósitos.

## Como Fazer:

Kotlin facilita o descarregamento de uma página da web por meio de suas bibliotecas integradas. Veja um exemplo simples abaixo:

```Kotlin
import java.net.URL

fun main(args: Array<String>) {
    val conteudoDaPagina = URL("http://exemplo.com").readText()
    println(conteudoDaPagina)
}
```

Ao executar este código, o conteúdo da página "http://exemplo.com" será impresso na saída do console.

## Aprofundando

Ao descarregar páginas da web em Kotlin, estamos, de fato, rastreando a internet, uma prática comum desde os primeiros dias da web. O rastreamento da web foi usado para indexar páginas da web em mecanismos de busca ou arquivar páginas da web.

Existem alternativas ao método `readText()`, dependendo das suas necessidades específicas. Você pode estar interessado em analisar o HTML de uma página, caso em que bibliotecas como Jsoup ou HtmlCleaner podem ser úteis. Se você está apenas interessado em fazer requisições HTTP, poderia considerar usar bibliotecas como OkHttp ou Fuel.

Lembre-se, o descarregamento de páginas da web deve ser feito respeitando os termos de serviço do site que você está descarregando. Além disso, nem todos os sites gostam de ser rastreados e podem tomar medidas para prevenir tal atividade.

## Ver Também

- [Jsoup](https://jsoup.org/)
- [HtmlCleaner](https://htmlcleaner.sourceforge.io/)
- [OkHttp](https://square.github.io/okhttp/)
- [Fuel](https://github.com/kittinunf/fuel)