---
title:                "Pesquisando e substituindo texto"
date:                  2024-02-01T22:01:31.363877-07:00
model:                 gpt-4-0125-preview
simple_title:         "Pesquisando e substituindo texto"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pt/vba/searching-and-replacing-text.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## O Que & Por Quê?

Pesquisar e substituir texto em Visual Basic for Applications (VBA) é essencial para editar documentos, planilhas e bancos de dados programaticamente. Essa capacidade permite que programadores automatizem edições em massa, corrigindo erros ou atualizando informações em grandes conjuntos de dados sem intervenção manual.

## Como fazer:

No VBA, pesquisar e substituir texto pode ser alcançado usando a função `Replace` ou por meio de modelos de objetos específicos em aplicações como Excel ou Word. Abaixo estão exemplos que ilustram ambas as abordagens.

### Usando a Função `Replace`:

A função `Replace` é direta para substituições simples de texto. Ela tem a forma `Replace(expressão, encontrar, substituirPor[, início[, contagem[, comparar]]])`.

Exemplo:
```vb
Dim originalText As String
Dim newText As String

originalText = "Olá, Mundo! Programar em VBA é divertido."
newText = Replace(originalText, "Mundo", "Todos")

Debug.Print newText
```
Saída:
```
Olá, Todos! Programar em VBA é divertido.
```

### Pesquisando e Substituindo no Excel:

Para o Excel, você pode usar o método `Range.Replace` que oferece mais controle, como sensibilidade a maiúsculas e minúsculas e substituições de palavras inteiras.

Exemplo:
```vb
Sub ReplaceTextInExcel()
    Dim ws As Worksheet
    Set ws = ThisWorkbook.Sheets("Folha1")

    Com ws.Range("A1:A100") ' Definir o intervalo onde você deseja pesquisar
        .Replace What:="antigo", Replacement:="novo", MatchCase:=False, LookAt:=xlPart
    End With
End Sub
```

### Pesquisando e Substituindo no Word:

De forma semelhante, o Word tem um recurso `Find` e `Replace` poderoso acessível através do VBA.

Exemplo:
```vb
Sub ReplaceTextInWord()
    Dim doc As Document
    Set doc = ActiveDocument
    
    Com doc.Content.Find
        .Text = "específico"
        .Replacement.Text = "particular"
        .Execute Replace:=wdReplaceAll
    End With
End Sub
```

## Aprofundamento:

Pesquisar e substituir texto no VBA remonta às capacidades de automação iniciais nas aplicações do Microsoft Office, aumentando significativamente a produtividade ao scriptar tarefas repetitivas. Com o tempo, essas funções evoluíram para se tornarem mais poderosas e flexíveis, atendendo a uma ampla gama de casos de uso.

Enquanto a função `Replace` do VBA é conveniente para operações simples de texto, os modelos de objeto do Excel e Word oferecem maior controle e devem ser usados para tarefas específicas de aplicativos. Eles suportam recursos avançados como correspondência de padrões, preservação de formatação e critérios de pesquisa nuances (por exemplo, combinar caso, palavras inteiras).

No entanto, o VBA e suas capacidades de manipulação de texto, embora robustos dentro do ecossistema Microsoft, podem não ser sempre a melhor ferramenta para necessidades de processamento de texto de alto desempenho ou mais complexas. Linguagens como Python, com bibliotecas como `re` para expressões regulares, oferecem opções de manipulação de texto mais poderosas e versáteis. Mas para aqueles já trabalhando dentro de aplicações Microsoft Office, o VBA permanece uma escolha acessível e eficaz para automatizar tarefas de pesquisa e substituição.
