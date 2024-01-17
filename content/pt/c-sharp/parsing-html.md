---
title:                "Análise de html"
html_title:           "C#: Análise de html"
simple_title:         "Análise de html"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## O que & Por quê?

Fazer o parsing (análise) de HTML é um processo de extrair informações específicas de uma linguagem de marcação usada para criar páginas da web. Os programadores geralmente fazem isso para facilitar o acesso e o processamento dessas informações, economizando tempo e trabalho manual.

## Como fazer:

```C#
using System;
using HtmlAgilityPack; // Biblioteca para análise de HTML

string url = "https://www.example.com/"; // URL do site a ser analisado

// Criando objeto do tipo HtmlWeb para fazer o download do conteúdo da página
var web = new HtmlWeb();
var doc = web.Load(url); // Carrega o conteúdo da página em um objeto do tipo HtmlDocument

// Usando o método SelectNodes para selecionar todos os elementos com a tag "h1"
var headings = doc.DocumentNode.SelectNodes("//h1");

foreach (var heading in headings)
{
    Console.WriteLine(heading.InnerText); // Imprime o conteúdo de cada elemento selecionado
}
```

Output:
```
Título da Página 1
Título da Página 2
Título da Página 3
```

## Mergulho Profundo:

Fazer parsing de HTML é uma tarefa comum para desenvolvedores da web, pois permite que eles extraiam informações úteis dos sites para uso em seus projetos. Antes da popularidade dos frameworks e bibliotecas para análise de HTML, esse processo era feito manualmente, o que consumia muito tempo e esforço. No entanto, hoje existem várias opções, como o HtmlAgilityPack, que facilitam esse processo.

## Veja Também:

- [Documentação do HtmlAgilityPack](https://html-agility-pack.net/)
- [Comparação entre diferentes bibliotecas de análise de HTML em C#](https://www.codeproject.com/Articles/659019/A-Performance-Comparison-of-HTML-Parsers)