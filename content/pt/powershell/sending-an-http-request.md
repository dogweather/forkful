---
title:                "Enviando uma solicitação http"
html_title:           "Bash: Enviando uma solicitação http"
simple_title:         "Enviando uma solicitação http"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/powershell/sending-an-http-request.md"
---

{{< edit_this_page >}}

## O que e por que?

Enviar uma solicitação HTTP é pedir informações a um servidor da web. Programadores fazem isso para interagir com APIs, buscar dados, enviar dados, entre outros.

## Como fazer:

Para enviar uma solicitação HTTP no PowerShell, utilizamos o cmdlet `Invoke-WebRequest`. Aqui está um exemplo simples:

```PowerShell
$url = "http://api.exemplo.com"
$response = Invoke-WebRequest -Uri $url
echo $response.Content
```
Isso chamará a URL e imprimirá a resposta do conteúdo.

## Mergulho profundo

Historicamente, enviávamos solicitações HTTP em PowerShell usando a classe .NET `System.Net.WebRequest`, mas desde o PowerShell 3.0, temos o cmdlet `Invoke-WebRequest` muito mais amigável.

Se você precisar de uma alternativa ao `Invoke-WebRequest`, pode considerar o `Invoke-RestMethod`, que analisa a resposta diretamente para você quando possível.

Ao enviar uma solicitação HTTP, o sistema cria uma conexão TCP com o servidor web, envia os detalhes da solicitação (ou seja, GET, POST, cabeçalhos, etc.), e então espera e coleta a resposta. O `Invoke-WebRequest` abstrai a maior parte disso para você, mas é importante saber o que está acontecendo nos bastidores.

## Veja também

1. [Documentação oficial do Microsoft PowerShell](https://docs.microsoft.com/pt-br/powershell/)
3. [Página da web do PowerShell cmdlet Invoke-WebRequest](https://docs.microsoft.com/pt-br/powershell/module/microsoft.powershell.utility/invoke-webrequest?view=powershell-7.1)

Lembre-se, a prática leva à perfeição. Continue codificando e explorando!