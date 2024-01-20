---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "C#"
category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

# Web Scraping com C#: Como Analisar HTML 

## O Que & Por Quê?

Analisar HTML significa extrair dados de um documento HTML. Programadores fazem isso para coletar informações úteis de sites, tendo em vista que muitos recursos de dados estão disponíveis nesse formato.

## Como Fazer:

Vamos usar a biblioteca `HtmlAgilityPack`. Para instalar, use o seguinte comando no Package Manager Console: 

```C#
PM> Install-Package HtmlAgilityPack
```

Agora, aqui está um exemplo simples para extrair links de uma página da web:

```C#
using HtmlAgilityPack;
using System;
using System.Linq;

public class Program
{
    public static void Main()
    {
        var webGet = new HtmlWeb();
        var document = webGet.Load("https://www.example.com");
        var nodes = document.DocumentNode.SelectNodes("//a[@href]");

        foreach(var node in nodes)
        {
             Console.WriteLine(node.InnerHtml);
        }
    }
}
```

Este código irá imprimir todos os links (tag 'a') presentes na página "https://www.example.com".

## Mergulho Profundo:

(1) **Contexto Histórico**: A biblioteca `HtmlAgilityPack` surgiu em um contexto onde a análise de HTML não era uma tarefa fácil para os desenvolvedores de C#. Mesmo as bibliotecas existentes, na época, não conseguiam interpretar corretamente a estrutura aninhada de elementos HTML, ignorando muitas vezes os erros de sintaxe encontrados nas páginas da web.

(2) **Alternativas**: Algumas outras bibliotecas para analisar HTML em C# incluem `AngleSharp` e `CsQuery`. Ambas são ótimas opções, cada uma com seus respectivos recursos.

(3) **Detalhes de Implementação**: `HtmlAgilityPack` ignora erros de sintaxe e pode analisar até mesmo HTML malformado. Ela opera em uma arquitetura de pilhas de leitura, dando-lhe a capacidade de fazer buscas complexas e manipulações ao mesmo tempo em que mantém a performance.

## Veja Também:

* [Documentação oficial `HtmlAgilityPack`](https://html-agility-pack.net/)
* [`AngleSharp` no GitHub](https://github.com/AngleSharp/AngleSharp)
* [`CsQuery` no GitHub](https://github.com/jamietre/CsQuery)

----

Lembre-se, a prática é a chave para se tornar um mestre na análise de HTML com C#. Comece pequeno, desmonte um site de cada vez, e veja até onde você pode ir.