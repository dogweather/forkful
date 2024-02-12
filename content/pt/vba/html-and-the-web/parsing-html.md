---
title:                "Analisando HTML"
aliases:
- /pt/vba/parsing-html.md
date:                  2024-02-01T21:57:40.397957-07:00
model:                 gpt-4-0125-preview
simple_title:         "Analisando HTML"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/vba/parsing-html.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Analisar HTML no Visual Basic for Applications (VBA) envolve extrair informações específicas de um documento HTML. Programadores fazem isso para automatizar o processo de leitura e manipulação de dados de páginas da web, como raspagem de conteúdo do site ou automação de envios de formulário e recuperação de dados, dentro de aplicações como Microsoft Excel ou Access que suportam VBA.

## Como:

No VBA, você pode analisar HTML usando a `Biblioteca de Objetos HTML da Microsoft`. Adicione uma referência a esta biblioteca no seu editor VBA indo em Ferramentas > Referências e marcando `Biblioteca de Objetos HTML da Microsoft`. Isso te dá acesso a classes para navegar e manipular documentos HTML.

Aqui está um exemplo simples que mostra como carregar um documento HTML de um arquivo e extrair todos os links (tags de âncora):

```vb
Sub ParseHTML()
    Dim htmlDoc As MSHTML.HTMLDocument
    Dim htmlElement As MSHTML.IHTMLElement
    Dim htmlElements As MSHTML.IHTMLElementCollection
    Dim htmlFile As String
    Dim fileContent As String
    
    ' Carregar o conteúdo HTML de um arquivo
    htmlFile = "C:\caminho\para\seu\arquivo.html"
    Open htmlFile For Input As #1
    fileContent = Input$(LOF(1), 1)
    Close #1
    
    ' Inicializar Documento HTML
    Set htmlDoc = New MSHTML.HTMLDocument
    htmlDoc.body.innerHTML = fileContent
    
    ' Pegar todas as tags de âncora
    Set htmlElements = htmlDoc.getElementsByTagName("a")

    ' Loop por todos os elementos de âncora e imprimir o atributo href
    For Each htmlElement In htmlElements
        Debug.Print htmlElement.getAttribute("href")
    Next htmlElement
End Sub
```

Este script lê o conteúdo de um arquivo HTML, carrega-o em um objeto `HTMLDocument`, recupera todos os elementos de âncora (`<a>` tags) e, em seguida, itera sobre eles, imprimindo o atributo `href` de cada um na Janela Imediata.

## Aprofundamento:

Historicamente, analisar HTML no VBA tem sido um pouco complicado devido à falta de suporte direto para tecnologias modernas de raspagem da web e manipulação de documentos. A Biblioteca de Objetos HTML da Microsoft, apesar de ser poderosa, é um pouco datada e pode não lidar com padrões web modernos tão suavemente quanto tecnologias mais recentes.

Para tarefas complexas de análise de HTML e raspagem da web, ferramentas e linguagens alternativas como Python com bibliotecas como Beautiful Soup ou Scrapy são frequentemente recomendadas. Essas ferramentas modernas oferecem mais flexibilidade, melhor desempenho e estão mais em sintonia com os padrões web atuais. No entanto, ao trabalhar dentro do ecossistema do Microsoft Office, usar VBA com a Biblioteca de Objetos HTML da Microsoft permanece uma habilidade valiosa. Isso desbloqueia a manipulação direta do conteúdo HTML de uma forma que se integra perfeitamente com aplicações como Excel e Access, fornecendo um método direto para realizar tarefas que envolvem manipulação básica de documentos HTML sem a necessidade de sair do ambiente familiar do VBA.
