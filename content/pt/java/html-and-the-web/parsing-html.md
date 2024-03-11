---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:22.695390-07:00
description: "Analisar HTML significa vasculhar a marca\xE7\xE3o para extrair dados\
  \ como texto, links ou outros elementos. Fazemos isso para interagir com ou raspar\
  \ conte\xFAdo\u2026"
lastmod: '2024-03-11T00:14:20.150601-06:00'
model: gpt-4-0125-preview
summary: "Analisar HTML significa vasculhar a marca\xE7\xE3o para extrair dados como\
  \ texto, links ou outros elementos. Fazemos isso para interagir com ou raspar conte\xFA\
  do\u2026"
title: Analisando HTML
---

{{< edit_this_page >}}

## O Que & Porquê?

Analisar HTML significa vasculhar a marcação para extrair dados como texto, links ou outros elementos. Fazemos isso para interagir com ou raspar conteúdo da web, automatizar tarefas de navegação ou testar aplicativos web.

## Como Fazer:

Vamos usar o Jsoup, uma biblioteca prática para trabalhar com HTML do mundo real. Primeiro, adicione a dependência:

```xml
<dependency>
    <groupId>org.jsoup</groupId>
    <artifactId>jsoup</artifactId>
    <version>1.15.2</version>
</dependency>
```

Agora, para a parte divertida. Veja como capturar o título de uma página web e imprimi-lo:

```java
import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

public class AnalisadorHtml {
    public static void main(String[] args) throws IOException {
        String url = "http://example.com";
        Document doc = Jsoup.connect(url).get();
        String titulo = doc.title();
        System.out.println("Título: " + titulo);
    }
}
```

Saída:

```
Título: Exemplo de Domínio
```

Que tal extrair todos os links?

```java
import org.jsoup.nodes.Element;
import org.jsoup.select.Elements;

// ... dentro do main ou de outro método
Elements links = doc.select("a[href]");
for (Element link : links) {
    System.out.println("Link: " + link.attr("href"));
}
```

## Aprofundamento

Houve um tempo em que o HTML era domado por padrões de regex, um método tanto propenso a erros quanto pesadeloso para documentos complexos. Então chegou o Jsoup no final dos anos 2000, fornecendo uma interface semelhante à jQuery para Java para analisar, percorrer e manipular HTML.

O Jsoup não é a única escolha. Há o HtmlUnit para testes completos de aplicativos web com suporte a JavaScript, mas é mais pesado e complexo. Para tarefas leves, o Apache Commons Validator é ótimo apenas para extrair URLs.

Por baixo do capô, o Jsoup usa um parser DOM, que modela todo o documento na memória como uma árvore. Essa abordagem facilita a seleção e navegação na estrutura do HTML. Além disso, é tolerante com HTML desleixado, corrigindo problemas na mosca para garantir uma análise robusta.

Lembre-se, ao raspar, sempre verifique o `robots.txt` do site e os termos de serviço para evitar problemas legais ou ser banido pelo IP.

## Veja Também

- Documentação Oficial do Jsoup: https://jsoup.org/
- HtmlUnit: http://htmlunit.sourceforge.net/
- Apache Commons Validator: https://commons.apache.org/proper/commons-validator/
