---
title:                "Analisando html"
html_title:           "C#: Analisando html"
simple_title:         "Analisando html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## Por que

Se você está desenvolvendo um aplicativo que precisa acessar dados de uma página da web, é muito provável que você precise realizar a análise de HTML. A análise de HTML é a capacidade de extrair informações específicas de uma página da web e convertê-las em um formato útil para o seu aplicativo.

## Como fazer

A análise de HTML pode ser realizada em C# de várias maneiras, mas vamos nos concentrar em uma das bibliotecas mais populares, chamada HtmlAgilityPack. Esta biblioteca oferece uma ampla gama de funcionalidades para facilitar a análise de HTML, incluindo a capacidade de navegar pela estrutura do documento HTML e extrair elementos específicos.

Para começar, certifique-se de ter instalado o HtmlAgilityPack em seu projeto. Em seguida, importe a biblioteca no seu código:

```C#
using HtmlAgilityPack;
```

Em seguida, você precisará obter o conteúdo HTML da página que deseja analisar. Isso pode ser feito usando uma biblioteca de solicitações HTTP, como o HttpClient. Uma vez que você tiver o conteúdo HTML, você pode carregá-lo em um objeto HtmlDocument do HtmlAgilityPack:

```C#
var url = "https://www.example.com";
var httpClient = new HttpClient();
var html = await httpClient.GetStringAsync(url);

var doc = new HtmlDocument();
doc.LoadHtml(html);
```

Com o documento HTML carregado, você pode usar métodos do HtmlAgilityPack, como `SelectSingleNode()` e `SelectNodes()`, para extrair informações específicas do documento. Por exemplo, se você quiser obter o título da página, pode usar o método `SelectSingleNode()` para encontrar o elemento `title` e, em seguida, acessar seu conteúdo:

```C#
var title = doc.DocumentNode.SelectSingleNode("//title")?.InnerText;
```

Você também pode usar expressões XPath para selecionar elementos específicos com base em seu ID, classe, tag, entre outros critérios. Por exemplo, se você quiser obter todos os links em uma página, pode usar a expressão `//a`:

```C#
var links = doc.DocumentNode.SelectNodes("//a");
foreach (var link in links)
{
    Console.WriteLine(link.GetAttributeValue("href", ""));
}
```

## Mergulho Profundo

O HtmlAgilityPack também oferece a capacidade de modificar e salvar o documento HTML. Você pode adicionar, excluir ou modificar elementos de acordo com suas necessidades. Além disso, a biblioteca também é compatível com o uso de CSS selectors, o que facilita ainda mais a seleção de elementos específicos.

Também é importante lembrar que a análise de HTML pode ser uma tarefa demorada e consome recursos. Certifique-se de otimizar seu código para não sobrecarregar seu aplicativo ou o servidor da página que está sendo analisada.

## Veja Também

- Documentação do HtmlAgilityPack: https://html-agility-pack.net/
- Guia do C# para Análise de HTML: https://docs.microsoft.com/pt-br/dotnet/csharp/tutorials/manipulate-html-xml/?view=netcore-3.1