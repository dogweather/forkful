---
title:                "Analisando HTML"
html_title:           "Arduino: Analisando HTML"
simple_title:         "Analisando HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## Para que e por que?
Analisar o HTML é a prática de decodificar o conteúdo do HTML e transformá-lo em algo que você pode manipular e usar em seu código. Programadores fazem isso para extrair dados úteis de páginas da web e automatizar ações com base nessas informações. 

## Como fazer:
Vamos usar o módulo PowerShell HtmlAgilityPack. Primeiro, você precisará instalá-lo:

```PowerShell
Install-Package HtmlAgilityPack
```
Agora você pode importar o módulo usando o comando:

```PowerShell
Import-Module HtmlAgilityPack
```
Aqui está um exemplo de como analisar uma página da web:

```PowerShell
$pageUrl = 'http://exemplo.com'
$web = New-Object HtmlAgilityPack.HtmlWeb
$page = $web.Load($pageUrl)
$nodes = $page.DocumentNode.SelectNodes('//a[@class="some-class"]')

foreach($node in $nodes)
{
  Write-Host $node.InnerText
}
```

## Conhecimento profundo
1. Contexto histórico: No passado, a análise HTML era complicada e muitas vezes imprecisa, pois o HTML é uma linguagem de marcação, não estruturada. O advento de bibliotecas como o HtmlAgilityPack facilitou muito a tarefa.
2. Alternativas: Outros módulos podem ser usados para análise de HTML em PowerShell, como CsQuery e ScrapySharp.
3. Detalhes de implementação: HtmlAgilityPack permite análise de HTML, mesmo com tags mal formadas. Ele também suporta Linq para consultar nós, facilitando a manipulação de dados.

## Veja também
Aqui estão alguns links para recursos úteis sobre análise de HTML em PowerShell:
- [Documentação oficial do HtmlAgilityPack](https://html-agility-pack.net/)
- [Análise HTML com CsQuery](https://github.com/jamietre/CsQuery)
- [Análise HTML com ScrapySharp](https://github.com/rflechner/ScrapySharp)