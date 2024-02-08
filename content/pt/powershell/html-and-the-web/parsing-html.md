---
title:                "Analisando HTML"
date:                  2024-02-03T19:12:43.228141-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?
Analisar HTML no PowerShell é sobre desmembrar conteúdo HTML para extrair dados específicos ou automatizar tarefas relacionadas à web. Os programadores fazem isso para interagir com páginas da web, fazer raspagem de conteúdo web ou automatizar submissões de formulários e outras interações web sem a necessidade de um navegador.

## Como fazer:

O PowerShell não possui nativamente um analisador HTML dedicado, mas você pode utilizar o cmdlet `Invoke-WebRequest` para acessar e analisar conteúdos HTML. Para análises e manipulações mais complexas, o HtmlAgilityPack, uma biblioteca .NET popular, pode ser empregado.

### Usando `Invoke-WebRequest`:

```powershell
# Exemplo simples para buscar títulos de uma página da web
$response = Invoke-WebRequest -Uri 'http://example.com'
# Utilize a propriedade ParsedHtml para acessar elementos DOM
$title = $response.ParsedHtml.title
Write-Output $title
```

Saída de Exemplo:

```
Exemplo de Domínio
```

### Usando HtmlAgilityPack:

Primeiro, você precisa instalar o HtmlAgilityPack. Você pode fazer isso via Gerenciador de Pacotes NuGet:

```powershell
Install-Package HtmlAgilityPack -ProviderName NuGet
```

Então, você pode usá-lo no PowerShell para analisar HTML:

```powershell
# Carregar o assembly HtmlAgilityPack
Add-Type -Path "caminho\para\HtmlAgilityPack.dll"

# Criar um objeto HtmlDocument
$doc = New-Object HtmlAgilityPack.HtmlDocument

# Carregar HTML de um arquivo ou uma solicitação web
$htmlContent = (Invoke-WebRequest -Uri "http://example.com").Content
$doc.LoadHtml($htmlContent)

# Usar XPath ou outros métodos de consulta para extrair elementos
$node = $doc.DocumentNode.SelectSingleNode("//h1")

if ($node -ne $null) {
    Write-Output $node.InnerText
}
```

Saída de Exemplo:

```
Bem-vindo ao Example.com!
```

Nestes exemplos, `Invoke-WebRequest` é melhor para tarefas simples, enquanto o HtmlAgilityPack oferece um conjunto de recursos muito mais rico para análise e manipulação complexa de HTML.
