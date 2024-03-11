---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:43.228141-07:00
description: "Analisar HTML no PowerShell \xE9 sobre desmembrar conte\xFAdo HTML para\
  \ extrair dados espec\xEDficos ou automatizar tarefas relacionadas \xE0 web. Os\
  \ programadores\u2026"
lastmod: '2024-03-11T00:14:20.519130-06:00'
model: gpt-4-0125-preview
summary: "Analisar HTML no PowerShell \xE9 sobre desmembrar conte\xFAdo HTML para\
  \ extrair dados espec\xEDficos ou automatizar tarefas relacionadas \xE0 web. Os\
  \ programadores\u2026"
title: Analisando HTML
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
