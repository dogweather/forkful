---
title:                "Análise de HTML"
html_title:           "Java: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/java/parsing-html.md"
---

{{< edit_this_page >}}

## O que e Por que?

Parsing HTML (ou interpretacao de HTML) e o processo de analisar um documento HTML e extrair informacoes especificas dele. Esse processo e util para programadores pois permite que eles extraiam dados de paginas da web, como textos, imagens, links e outros elementos.

## Como fazer?

Para realizar o parsing de HTML em Java, precisamos usar uma biblioteca chamada Jsoup. Essa biblioteca nos permite manipular e analisar documentos HTML de forma simples e eficiente. Confira o codigo de exemplo abaixo para ver como isso e feito:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

public class HtmlParser {

    public static void main(String[] args) {

        // define a URL do site que queremos fazer parsing
        String url = "https://www.example.com";

        try {
            // conecta-se ao site e obtem o conteudo do HTML
            Document doc = Jsoup.connect(url).get();

            // obtem todos os links do documento HTML
            Elements links = doc.select("a[href]");

            // itera sobre os links e imprime o texto e a URL de cada um
            for (Element link : links) {
                System.out.println("Texto: " + link.text());
                System.out.println("URL: " + link.attr("abs:href"));
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Aqui esta um exemplo de saida do codigo acima:

```java
Texto: Exemplo de link
URL: https://www.example.com/link
Texto: Outro exemplo de link
URL: https://www.example.com/outro-link
```

## Mergulho Profundo

O parsing de HTML tem sido uma tarefa comum para programadores desde os primordios da internet, quando as paginas eram simples e nao tinham muita estrutura. Porem, com o passar dos anos, o HTML evoluiu e novas formas de extrair informacoes das paginas surgiram. Algumas alternativas para o parsing de HTML em Java incluem o uso de expressoes regulares e a biblioteca HTMLParser.

A biblioteca Jsoup, mencionada anteriormente, utiliza o modelo de documento DOM (Modelo de Objeto do Documento) para representar o conteudo HTML. Isso permite que os programadores naveguem e manipulem facilmente os elementos do documento.

## Veja Tambem

- [Documentacao do Jsoup](https://jsoup.org/)
- [Tutorial de Parsing de HTML com Jsoup](https://www.baeldung.com/java-ceiling-function)
- [Outra biblioteca para parsing de HTML em Java](http://htmlparser.sourceforge.net/)