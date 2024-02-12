---
title:                "Enviando uma solicitação HTTP"
aliases:
- /pt/vba/sending-an-http-request/
date:                  2024-02-01T22:01:56.522794-07:00
model:                 gpt-4-0125-preview
simple_title:         "Enviando uma solicitação HTTP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/vba/sending-an-http-request.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Que?

Enviar uma solicitação HTTP em Visual Basic for Applications (VBA) envolve acessar programaticamente recursos ou serviços web fazendo solicitações via HTTP. Programadores fazem isso para buscar dados, interagir com APIs online ou submeter formulários programaticamente a partir de suas aplicações habilitadas para VBA, como Excel, Access ou soluções customizadas em VBA.

## Como Fazer:

A chave para enviar uma solicitação HTTP em VBA é utilizando a biblioteca `Microsoft XML, v6.0` (ou versões anteriores, dependendo do seu sistema). Primeiro, garanta que esta referência está habilitada em seu projeto indo até Ferramentas > Referências no editor VBA e marcando `Microsoft XML, v6.0`.

Aqui está como enviar uma simples solicitação HTTP GET:

```vb
Dim httpRequest As Object
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")

With httpRequest
    .Open "GET", "https://api.example.com/data", False
    .send
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Erro: " & .Status & " - " & .statusText
    End If
End With
```

Para uma solicitação POST, onde precisamos enviar dados (por exemplo, JSON) para um servidor:

```vb
Dim httpRequest As Object, postData As String
Set httpRequest = CreateObject("MSXML2.XMLHTTP.6.0")
postData = "{""key"":""value""}"

With httpRequest
    .Open "POST", "https://api.example.com/submit", False
    .setRequestHeader "Content-Type", "application/json"
    .send postData
    If .Status = 200 Then
        Debug.Print .responseText
    Else
        Debug.Print "Erro: " & .Status & " - " & .statusText
    End If
End With
```

A saída de uma solicitação bem-sucedida pode ser uma string JSON ou uma página HTML, dependendo da API ou da página web com a qual você está interagindo:

```
{"data": "Esta é a resposta do servidor"}
```

## Aprofundamento

O método destacado utiliza o objeto `MSXML2.XMLHTTP`, parte dos Serviços Core XML da Microsoft (MSXML). Ele foi introduzido para oferecer aos desenvolvedores de VBA uma maneira de realizar operações baseadas em XML e, com o tempo, tornou-se uma ferramenta comum para solicitações HTTP, mesmo quando não trabalhando diretamente com dados XML. Apesar de sua idade, permanece uma opção confiável para interações web simples em VBA.

No entanto, o VBA e seus mecanismos de solicitação HTTP carecem da robustez e flexibilidade encontradas em ambientes de programação modernos. Por exemplo, lidar com solicitações assíncronas ou trabalhar dentro de aplicações que requerem recursos HTTP avançados (como websockets ou eventos enviados pelo servidor) está fora do escopo do VBA. Ao trabalhar em projetos de integração web mais complexos, desenvolvedores frequentemente recorrem a bibliotecas ou ferramentas externas, ou até automatizam comportamentos de navegador via técnicas de raspagem web, embora essas sejam soluções alternativas e não definitivas.

Linguagens e ambientes como Python com sua biblioteca `requests` ou JavaScript executado no Node.js oferecem capacidades de solicitação HTTP mais poderosas e versáteis diretamente da caixa, incluindo operações assíncronas, manipulação mais fácil de JSON e amplo suporte para diferentes tecnologias web. Desenvolvedores enraizados no ecossistema Microsoft podem considerar a transição para PowerShell ou C# para tarefas que exigem uma interação web mais sofisticada, aproveitando os extensos recursos de programação de rede do .NET.

Assim, enquanto as capacidades de solicitação HTTP do VBA são adequadas para consultas simples e tarefas de busca de dados, explorar alternativas se torna crucial à medida que as demandas do seu projeto evoluem em direção ao cenário web complexo e moderno.
