---
title:                "Enviando uma solicitação http com autenticação básica"
html_title:           "Clojure: Enviando uma solicitação http com autenticação básica"
simple_title:         "Enviando uma solicitação http com autenticação básica"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## O Que & Porquê?

Enviar um pedido HTTP com autenticação básica é o processo de enviar uma solicitação HTTP com cabeçalhos de autenticação. Programadores precisam disso para se comunicar com APIs que exigem nome de usuário e senha para acesso.

## Como Fazer:

Aqui está o código PowerShell básico para enviar um pedido HTTP com autenticação básica:

```PowerShell
$credenciais = New-Object System.Net.NetworkCredential("username", "senha")

$webRequest = [System.Net.WebRequest]::Create("http://api.url")
$webRequest.Credentials = $credenciais

$resposta = $webRequest.GetResponse()
```

A resposta do servidor será armazenada na variável `$resposta`.

## Aprofundamento:

Historicamente, a autenticação básica tem sido uma forma comum de autorizar o acesso a web APIs, embora outras opções estejam agora disponíveis.

Alternativas incluem a autenticação OAuth e tokens de acesso, que oferecem maior segurança.

Quando enviamos um pedido HTTP com autenticação básica, as credenciais são incluídas em um cabeçalho HTTP de "Autorização". O nome de usuário e senha são unidos por um dois-pontos e codificados em Base64.

## Veja Também:

1. [Autenticação básica na Wikipédia](https://pt.wikipedia.org/wiki/Autentica%C3%A7%C3%A3o_b%C3%A1sica_de_acesso)
2. [Documentação do System.Net.NetworkCredential](https://docs.microsoft.com/pt-br/dotnet/api/system.net.networkcredential?view=net-5.0)
3. [Autenticação OAuth na Wikipédia](https://pt.wikipedia.org/wiki/OAuth).