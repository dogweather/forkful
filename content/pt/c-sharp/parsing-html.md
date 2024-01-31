---
title:                "Análise de HTML"
date:                  2024-01-20T15:30:39.412208-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"

category:             "C#"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/c-sharp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Analisar HTML é desmembrar a estrutura e conteúdo de uma página web para manipulação ou extração de dados. Programadores fazem isso para interagir com web scraping, testar aplicações ou integrar sistemas que não oferecem APIs prontas.

## How to:
Para tratar HTML em C#, você pode usar a biblioteca `HtmlAgilityPack`. Vamos ver como pegar todos os links de uma página:

```C#
using System;
using HtmlAgilityPack;

class Program
{
    static void Main()
    {
        var web = new HtmlWeb();
        var doc = web.Load("http://exemplo.com");

        foreach (var link in doc.DocumentNode.SelectNodes("//a[@href]"))
        {
            Console.WriteLine(link.GetAttributeValue("href", ""));
        }
    }
}
```

Saída de exemplo (dependendo do conteúdo de `http://exemplo.com`):
```
http://exemplo.com/home
http://exemplo.com/about
http://exemplo.com/contact
```

## Deep Dive
A prática de analisar HTML remonta ao início da web, quando era uma ferramenta vital para data mining. Alternativas ao `HtmlAgilityPack` incluem `AngleSharp`, que é mais moderno e adere ao padrão mais recente do DOM.

Detalhes de implementação notáveis:
- A robustez na análise é crucial; HTML da vida real é frequentemente malformado.
- O `HtmlAgilityPack` pode lidar com tags não fechadas e automática correção.
- Interagir com o `XPath` facilita navegar pelo DOM do documento HTML.

Além de bibliotecas de parsing, outras formas de interação com HTML incluem APIs de browser como o `WebBrowser` control no .NET Framework para execução de JavaScript e manipulação de páginas dinâmicas.

## See Also
- HtmlAgilityPack no GitHub: https://github.com/zzzprojects/html-agility-pack
- Documentação do AngleSharp: https://anglesharp.github.io/
- Referência ao XPath: https://www.w3schools.com/xml/xpath_intro.asp
