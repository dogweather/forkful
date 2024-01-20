---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/parsing-html.md"
---

{{< edit_this_page >}}

# Análise de HTML com Java - Fácil e Engenhoso!

## O quê & Porquê?

Analisar HTML significa interpretar o código HTML para extrair dados específicos ou entender a sua estrutura. Os programadores fazem isso para recolher dados, integrar com outros serviços, testar a acessibilidade do site, entre outros.

## Como Fazer:

Vamos utilizar a biblioteca Jsoup. Ela fornece uma interface conveniente para manipulação e análise de HTML. Para adicionar Jsoup ao seu projeto, inclua a seguinte dependência no pom.xml do Maven;

```Java
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.11.3</version>
</dependency>
```

Aqui está um exemplo simples para recuperar o título de um site.

```Java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class Main {

    public static void main(String[] args) throws Exception {

        String url = "https://google.com";
        Document document = Jsoup.connect(url).get();

        String title = document.title();
        System.out.println("Title: " + title);
    }
}
```

A saída desse código será algo como;
 ```"Title: Google"```

## Mergulho Profundo:

- Contexto Histórico: A análise de HTML foi necessária desde a popularização da web. À medida que os sites se tornaram mais complexos, a necessidade de bibliotecas como o Jsoup aumentou.
- Alternativas: Além do Jsoup, existem outras bibliotecas como o HtmlUnit e o jtidy.
- Detalhes de Implementação: O Jsoup, por exemplo, é uma API Java que cria uma árvore de elementos (DOM) do documento HTML, que pode ser manipulada para extrair os dados.

## Veja Também:

- [Tutorial Jsoup Oficial](https://jsoup.org/cookbook/)
- [Introdução ao HtmlUnit (em inglês)](https://htmlunit.sourceforge.io/gettingStarted.html)
- [Documentação jtidy (em inglês)](https://jtidy.sourceforge.io)