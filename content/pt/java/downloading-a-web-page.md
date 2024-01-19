---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que é & Por quê?

Baixar uma página da web é o processo de obter todo o conteúdo HTML de um determinado URL. Programadores fazem isso para analisar e manipular esse conteúdo, seja para automatizar tarefas, raspagem de dados, testes ou qualquer outra utilidade.
 
## Como fazer:

Vamos usar a biblioteca Jsoup para baixar uma página web em Java. Você precisa instalar a biblioteca Jsoup no seu projeto antes.

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class Main {
    public static void main(String[] args) {
        try {
            Document doc = Jsoup.connect("http://example.com").get();
            System.out.println(doc.html());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Quando você roda isto, irá imprimir todo o conteúdo HTML da página `http://example.com`.

## Mergulho Profundo

Jsoup é um analisador de HTML baseado em Java, que foi lançado em 2010. Ele é fácil de usar e tem muitas funcionalidades que tornam a análise de HTML em Java muito mais fácil do que se você fizesse tudo manualmente.

Existe outra biblioteca popular, chamada HtmlUnit, que também é usada para baixar páginas web. No entanto, Jsoup é geralmente considerado superior em termos de facilidade de uso e qualidade de análise.

Para baixar uma página web, Jsoup faz uma solicitação HTTP GET ao URL especificado. Ele analisa a resposta e constrói uma árvore de documento, que você pode então percorrer e manipular.

## Veja Também

1. Jsoup official documentation: [here](https://jsoup.org/).
2. Wikipedia on Web Scraping: [here](https://pt.wikipedia.org/wiki/Coleta_de_dados_web).
3. Stack Overflow discussion on Jsoup vs HtmlUnit: [here](https://stackoverflow.com/questions/3152138/what-are-the-pros-and-cons-of-jsoup-and-htmlunit).