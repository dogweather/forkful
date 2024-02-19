---
aliases:
- /pt/vba/downloading-a-web-page/
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:52:43.726610-07:00
description: "Baixar uma p\xE1gina da web em Visual Basic for Applications (VBA) envolve\
  \ obter o conte\xFAdo HTML de uma p\xE1gina da web da Internet. Programadores\u2026"
lastmod: 2024-02-18 23:08:57.972489
model: gpt-4-0125-preview
summary: "Baixar uma p\xE1gina da web em Visual Basic for Applications (VBA) envolve\
  \ obter o conte\xFAdo HTML de uma p\xE1gina da web da Internet. Programadores\u2026"
title: "Baixando uma p\xE1gina da web"
---

{{< edit_this_page >}}

## O quê e Por quê?

Baixar uma página da web em Visual Basic for Applications (VBA) envolve obter o conteúdo HTML de uma página da web da Internet. Programadores frequentemente realizam essa tarefa para processar ou analisar o conteúdo de sites de forma programática, a partir do Excel, Access ou outros aplicativos do Office.

## Como fazer:

Para baixar uma página da web em VBA, você pode utilizar a biblioteca Microsoft XML, v6.0 (MSXML6), que permite solicitações HTTP do servidor. Antes de mergulhar no código, certifique-se de ter habilitado essa referência em seu editor VBA, indo em `Ferramentas` -> `Referências` e marcando `Microsoft XML, v6.0`.

Aqui está um exemplo simples de como baixar o conteúdo HTML de uma página da web:

```basic
Sub DownloadWebPage()
    Dim request As Object
    Dim url As String
    Dim response As String
    
    ' Inicialize o objeto de solicitação XML HTTP
    Set request = CreateObject("MSXML2.XMLHTTP")
    
    url = "http://www.example.com"
    
    ' Abre uma solicitação síncrona
    request.Open "GET", url, False
    
    ' Envie a solicitação para o servidor
    request.send
    
    ' Obtenha o texto da resposta
    response = request.responseText
    
    ' Saída da resposta para a janela imediata (para fins de depuração)
    Debug.Print response
    
    ' Limpeza
    Set request = Nothing
End Sub
```

Executar essa sub-rotina imprimirá o HTML de `http://www.example.com` na Janela Imediata no editor VBA. Note que o parâmetro `False` no método `Open` torna a solicitação síncrona, significando que o código esperará até que a página da web seja baixada antes de prosseguir para a próxima linha.

## Aprofundamento

A técnica mostrada depende do MSXML, a implementação pela Microsoft do padrão XML HTTP Request, frequentemente usado para solicitações AJAX em desenvolvimento web. Esse componente tem sido parte da pilha de tecnologias da Microsoft por um bom tempo, tornando-o uma escolha robusta para solicitações de rede em VBA.

No entanto, a dependência do MSXML e do VBA para baixar e analisar o conteúdo da web pode ser limitante, particularmente com aplicações web modernas que usam pesadamente o JavaScript para renderização de conteúdo dinâmico. Essas limitações podem fazer com que outras linguagens ou ferramentas como Python com bibliotecas como BeautifulSoup ou Selenium sejam mais adequadas para tarefas de raspagem da web devido à sua capacidade de executar JavaScript e lidar com interações complexas de sites.

Apesar disso, para tarefas simples que envolvem a busca de conteúdo HTML direto ou ao trabalhar dentro dos limites das aplicações do Office, o VBA permanece uma ferramenta prática. Sua integração dentro da suíte Office permite a manipulação direta de documentos baseados em conteúdo da web, oferecendo uma vantagem única para casos de uso específicos.
