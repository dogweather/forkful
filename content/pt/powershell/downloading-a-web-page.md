---
title:                "Baixando uma página da web"
html_title:           "Bash: Baixando uma página da web"
simple_title:         "Baixando uma página da web"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Na programação, o download de uma página web se refere a acessar e salvar seu conteúdo para o uso posterior. Esse recurso é essencial para web crawling, scraping de dados, testes automatizados, e mais.

## Como Fazer:

Podemos usar o cmdlet `Invoke-WebRequest` no PowerShell para efetuar o download de uma página web. Veja o exemplo abaixo:

```PowerShell
$url = 'https://www.exemplo.com'
$resposta = Invoke-WebRequest -Uri $url
$resposta.Content | Out-File -FilePath .\pagina.html
```

Neste exemplo, a URL direciona para o site que você quer baixar. A variável `$resposta` recebe os dados devolvidos pelo `Invoke-WebRequest`. Depois, a propriedade `$resposta.Content` é encaminhada para o comando `Out-File`, que cria um novo arquivo HTML e escreve os dados nele.

## Mergulhando Fundo

Historicamente, as páginas web eram baixadas usando um navegador ou um utilitário de linha de comando como o `wget` do UNIX. Com a evolução do PowerShell, o método `Invoke-WebRequest` se tornou a forma padrão de fazer isso no ambiente Windows.

Há diversas alternativas ao `Invoke-WebRequest`, como o `WebClient` de .NET e o `wget` do UNIX. Essas opções podem ser preferíveis, dependendo dos requisitos e das limitações do seu projeto.

O `Invoke-WebRequest` processa a página web baixada e a disponibiliza de forma acessível para você como um objeto 'HtmlWebResponseObject'. Esta funcionalidade é um dos pontos mais fortes do `Invoke-WebRequest`, porque simplifica a tarefa de manipular a página baixada.

## Veja também:

- Documentação do Microsoft PowerShell `Invoke-WebRequest`: [link](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7)