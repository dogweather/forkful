---
title:                "Análise de HTML"
date:                  2024-01-20T15:33:43.017909-07:00
html_title:           "Bash: Análise de HTML"
simple_title:         "Análise de HTML"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/parsing-html.md"
---

{{< edit_this_page >}}

## O Quê e Por Quê?
Parsear HTML é transformar o código HTML em algo que o PowerShell possa entender e manipular. Programadores fazem isso para extrair dados de websites, automatizar testes em páginas da web, ou manipular conteúdo de sites de forma programática.

## Como Fazer:
Para começar, você precisará de um módulo chamado `HtmlAgilityPack`. Este módulo não é nativo do PowerShell e precisará ser instalado. Use o seguinte comando:

```PowerShell
Install-Package HtmlAgilityPack
```

Depois de instalado, você pode usá-lo para parsear HTML. Aqui está um exemplo básico:

```PowerShell
# Adiciona o HtmlAgilityPack ao seu script
Add-Type -Path "C:\caminho\para\HtmlAgilityPack.dll"

# Define o endereço do website que você quer parsear
$url = 'http://exemplo.com'

# Baixa o HTML da página
$html = Invoke-WebRequest -Uri $url

# Carrega o HTML para o HtmlAgilityPack parsear
$doc = New-Object HtmlAgilityPack.HtmlDocument
$doc.LoadHtml($html.Content)

# Extrai todos os nós de 'a' (links)
$nodes = $doc.DocumentNode.SelectNodes('//a')

foreach ($node in $nodes)
{
    Write-Host "Texto do Link: $($node.InnerText.Trim())"
    Write-Host "Href: $($node.Attributes['href'].Value)"
}
```

Output de exemplo:
```
Texto do Link: Página Inicial
Href: http://exemplo.com/home
```

## Mergulho Profundo:
Parsear HTML no PowerShell não era uma tarefa fácil até pouco tempo atrás. Tradicionalmente, a manipulação de HTML era feita com linguagens especializadas em web como JavaScript ou com ferramentas de server-side como PHP. No entanto, o crescimento de scripts de automação e testing fez com que essa necessidade se expandisse para outras linguagens.

Antes do `HtmlAgilityPack`, algumas tarefas requeriam uso de regex para extrair informações do HTML, um método propenso a falhas devido à complexidade do HTML. Um conceito importante ao parsear HTML é entender que HTML é um documento estruturado e não apenas texto. O `HtmlAgilityPack` traz essa facilidade, funcionando como um parser de XML, o que é perfeito para HTML, que é essencialmente uma forma de XML.

Além disso, tecnologias como `Invoke-WebRequest` e `ConvertFrom-Json` no PowerShell permitem interações web de alto nível. Outras alternativas incluem o uso de APIs, webscraping com outras linguagens, ou utilizando ferramentas como o Selenium para interações mais complexas com páginas web.

## Veja Também:

- Documentação do HtmlAgilityPack: [html-agility-pack.net/](https://html-agility-pack.net/)
- PowerShell Gallery: [powershellgallery.com/](https://www.powershellgallery.com/)
- Selenium WebDriver: [selenium.dev](https://www.selenium.dev/)
