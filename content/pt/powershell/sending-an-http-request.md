---
title:                "Enviando uma solicitação http"
html_title:           "PowerShell: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que e porque?

Enviar uma solicitação HTTP é uma maneira de um programa se comunicar com um servidor da web. Isso é útil para coletar informações, enviar dados ou executar outras ações em um site. Programadores usam solicitações HTTP para automatizar tarefas, integrar sistemas e criar aplicativos que interagem com a web de forma eficiente.

## Como fazer:

```PowerShell
# Exemplo básico:
Invoke-WebRequest -Uri "https://www.meusite.com"

# Passando parâmetros:
$Params = @{Param1 = 'valor1'; Param2 = 'valor2'}
Invoke-WebRequest -Uri "https://www.meusite.com/api" -Method POST -Body $Params

# Autenticação:
$Cred = Get-Credential
Invoke-WebRequest -Uri "https://www.meusite.com/api" -Credential $Cred
```

Saída de exemplo:

```
StatusCode        : 200
StatusDescription : OK
Content           : <html>
                      <head>
                        <title>Meu Site</title>
                      </head>
                      <body>
                        <h1>Bem-vindo ao meu site!</h1>
                      </body>
                    </html>
RawContent        : HTTP/1.1 200 OK
                    Content-Type: text/html
                    Date: Sat, 01 May 2021 00:00:00 GMT
                    
                    <html>
                      <head>
                        <title>Meu Site</title>
                      </head>
                      <body>
                        <h1>Bem-vindo ao meu site!</h1>
                      </body>
                    </html>
Forms             : {}
Headers           : {[Content-Type, text/html], [Date, Sat, 01 May 2021 00:00:00 GMT]}
Images            : {}
InputFields       : {}
Links             : {}
ParsedHtml        : <html>
                      <head>
                        <title>Meu Site</title>
                      </head>
                      <body>
                        <h1>Bem-vindo ao meu site!</h1>
                      </body>
                    </html>
RawContentLength  : 132
```

## Deep Dive:

Enviar solicitações HTTP é uma técnica amplamente utilizada em programação web. Antigamente, os programadores precisavam escrever seu próprio código para lidar com as comunicações HTTP, mas agora existem muitas bibliotecas e frameworks que simplificam o processo, como o PowerShell. Alternativas para enviar solicitações HTTP incluem usar um navegador da web ou uma ferramenta de linha de comando como o cURL. O PowerShell usa o cmdlet "Invoke-WebRequest" para fazer as solicitações e retorna uma resposta que contém informações como o status, os cabeçalhos e o conteúdo da resposta.

## Veja também:

- Documentação do cmdlet "Invoke-WebRequest": https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/invoke-webrequest
- Exemplos de uso do cmdlet "Invoke-WebRequest": https://dbatools.io/commands/invoke-webrequest/