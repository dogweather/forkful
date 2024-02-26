---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:02:27.356306-07:00
description: "Enviar uma solicita\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica em\
  \ Visual Basic for Applications (VBA) trata de acessar recursos da web que s\xE3\
  o protegidos por\u2026"
lastmod: '2024-02-25T18:49:44.031976-07:00'
model: gpt-4-0125-preview
summary: "Enviar uma solicita\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica em Visual\
  \ Basic for Applications (VBA) trata de acessar recursos da web que s\xE3o protegidos\
  \ por\u2026"
title: "Enviando uma solicita\xE7\xE3o HTTP com autentica\xE7\xE3o b\xE1sica"
---

{{< edit_this_page >}}

## O Que & Por Quê?

Enviar uma solicitação HTTP com autenticação básica em Visual Basic for Applications (VBA) trata de acessar recursos da web que são protegidos por credenciais de nome de usuário e senha. Programadores fazem isso para interagir com APIs seguras ou serviços web dentro de suas aplicações movidas a VBA, como automatizar tarefas no Excel ou Access com dados de pontos finais seguros.

## Como fazer:

No VBA, você pode usar a biblioteca `Microsoft XML, v6.0` (MSXML2) para enviar solicitações HTTP com autenticação básica. Isso envolve definir o cabeçalho `"Authorization"` da solicitação para incluir as credenciais num formato codificado em base64. Aqui está um guia passo a passo:

1. **Referenciar MSXML2**: Primeiro, garanta que seu projeto VBA referencie a biblioteca `Microsoft XML, v6.0`. No editor do VBA, vá em Ferramentas > Referências e marque `Microsoft XML, v6.0`.

2. **Criar e enviar a solicitação HTTP**: Use o seguinte trecho de código VBA como guia. Substitua `"your_username"` e `"your_password"` pelas suas credenciais reais e ajuste a URL conforme necessário.

    ```vb
    Dim XMLHttp As Object
    Set XMLHttp = CreateObject("MSXML2.XMLHTTP")
    Dim url As String
    url = "http://example.com/api/resource" ' Substitua pela URL real
    Dim base64Credentials As String
    base64Credentials = EncodeBase64("your_username:your_password")
    
    XMLHttp.Open "GET", url, False
    XMLHttp.setRequestHeader "Authorization", "Basic " & base64Credentials
    XMLHttp.send
    
    Debug.Print XMLHttp.responseText ' Saída da resposta para a Janela Imediata
    ```

3. **Codificar credenciais em base64**: O VBA não possui uma função interna para codificação em base64, mas você pode usar esta função personalizada `EncodeBase64`:

    ```vb
    Function EncodeBase64(text As String) As String
        Dim arrData() As Byte
        arrData = StrConv(text, vbFromUnicode)
        
        Dim objXML As MSXML2.DOMDocument60
        Dim objNode As MSXML2.IXMLDOMElement
        
        Set objXML = New MSXML2.DOMDocument60
        Set objNode = objXML.createElement("b64")
        
        objNode.dataType = "bin.base64"
        objNode.nodeTypedValue = arrData
        EncodeBase64 = objNode.Text
    End Function
    ```
    
Isso enviará uma solicitação GET para `http://example.com/api/resource` com as credenciais de autenticação básica especificadas, e imprimirá a resposta.

## Aprofundamento

A abordagem usada aqui, embora eficaz para casos de uso simples, depende do esquema de Autenticação Básica, que envia credenciais num formato facilmente decodificável (a codificação em base64 não é criptografia). Devido à sua vulnerabilidade, especialmente em contextos não HTTPS, a Autenticação Básica não é recomendada para transmitir dados sensíveis pela internet sem camadas de segurança adicionais como SSL/TLS.

Historicamente, a Autenticação Básica foi um dos primeiros métodos desenvolvidos para controlar o acesso a recursos da web. Hoje em dia, padrões de autenticação mais seguros e flexíveis, como o OAuth 2.0, são geralmente preferidos para novas aplicações. Dadas as limitações do VBA e as dependências externas necessárias para métodos de autenticação mais avançados, os desenvolvedores muitas vezes empregam VBA em ambientes internos ou menos críticos para segurança, ou usam-no como um degrau para prototipar ideias rapidamente.

Ao usar o VBA para solicitações HTTP, lembre-se de que cada versão da biblioteca MSXML pode suportar diferentes recursos e padrões de segurança. Sempre use a versão mais recente compatível com sua aplicação para garantir melhor segurança e desempenho. Além disso, considere as limitações ambientais e potenciais recursos obsoletos ao escolher VBA para novos projetos, especialmente aqueles que requerem comunicações HTTP seguras. Outros ambientes de programação ou linguagens podem oferecer soluções mais robustas, seguras e sustentáveis para tarefas semelhantes.
