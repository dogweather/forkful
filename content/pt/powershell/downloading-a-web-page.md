---
title:                "Baixando uma página da web"
html_title:           "PowerShell: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O que e por que?

Baixar uma página da web é quando um programa obtém o conteúdo de uma página da internet e o armazena em um arquivo local. Os programadores fazem isso para acessar informações de websites e usá-las em seus projetos ou para analisar e extrair dados.

## Como fazer:

```PowerShell
# Para baixar uma página da web usando o PowerShell, use o comando Invoke-WebRequest.
# Por exemplo, para baixar o código fonte de um website:
$page = Invoke-WebRequest -Uri "https://www.example.com"
$page.Content | Out-File -FilePath "exemplo.html"

# Você também pode baixar uma parte específica da página, como uma imagem:
$url = "https://www.example.com/image.jpg"
$output = "imagem.jpg"
(Invoke-WebRequest -Uri $url).RawContentStream | Out-File -FilePath $output -Encoding Byte
```

## Exemplo de saída:

Ao executar o primeiro comando no bloco de código acima, um arquivo chamado "exemplo.html" será criado no diretório atual contendo o código fonte da página da web baixada.

## Profundando:

Desde sua introdução em 2006, o PowerShell tornou-se uma ferramenta popular para automação de tarefas em sistemas Windows. Anteriormente, os programadores usavam outras linguagens, como Perl ou Python, para baixar páginas da web, mas agora isso pode ser facilmente realizado com o PowerShell. Existem também outras ferramentas e bibliotecas disponíveis para baixar conteúdo da web, como o cURL ou o HttpClient do .NET.

O comando Invoke-WebRequest do PowerShell também pode ser usado para interagir com APIs (interfaces de programação de aplicativos) de websites, o que é útil para automatizar a obtenção de dados em tempo real.

## Veja também:

- [Documentação do PowerShell sobre o comando Invoke-WebRequest](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)
- [Guia de uso do cURL para baixar páginas da web](https://curl.se/docs/http-scripting.html)
- [Biblioteca HttpClient do .NET](https://docs.microsoft.com/pt-br/dotnet/api/system.net.http.httpclient?view=net-5.0)