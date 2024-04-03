---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:20.091395-07:00
description: "Como fazer: O VBA oferece diversos m\xE9todos para escrever em um arquivo,\
  \ mas uma das maneiras mais diretas \xE9 usando o `FileSystemObject`. Aqui est\xE1\
  \ um guia\u2026"
lastmod: '2024-03-13T22:44:46.432244-06:00'
model: gpt-4-0125-preview
summary: "O VBA oferece diversos m\xE9todos para escrever em um arquivo, mas uma das\
  \ maneiras mais diretas \xE9 usando o `FileSystemObject`."
title: Escrevendo um arquivo de texto
weight: 24
---

## Como fazer:
O VBA oferece diversos métodos para escrever em um arquivo, mas uma das maneiras mais diretas é usando o `FileSystemObject`. Aqui está um guia passo a passo para criar um arquivo de texto simples e escrever dados nele:

1. **Referenciar o Microsoft Scripting Runtime**: Primeiro, certifique-se de que seu editor VBA tem acesso ao `FileSystemObject`. Vá em Ferramentas > Referências no editor VBA e marque "Microsoft Scripting Runtime."

2. **Criar um Arquivo de Texto**: O seguinte trecho de código VBA demonstra como criar um arquivo de texto e escrever uma linha de texto nele.

```vb
Sub WriteToFile()
    Dim fso As FileSystemObject
    Set fso = New FileSystemObject
    
    Dim textFile As Object
    ' Parâmetros CreateTextFile: (NomeArquivo, Sobrescrever, Unicode)
    Set textFile = fso.CreateTextFile("C:\seuCaminho\exemplo.txt", True, False)
    
    ' Escrever uma linha de texto
    textFile.WriteLine "Olá, VBA!"
    
    ' Fechar o arquivo
    textFile.Close
End Sub
```

Este script cria (ou sobrescreve se já existir) um arquivo nomeado `exemplo.txt` no diretório especificado e escreve "Olá, VBA!" nele antes de fechar o arquivo para salvar as alterações.

3. **Saída de Exemplo**:

Após executar o script VBA acima, você encontrará um arquivo nomeado `exemplo.txt` com o seguinte conteúdo:

```
Olá, VBA!
```

## Aprofundando:
O `FileSystemObject` (FSO), parte da biblioteca Microsoft Scripting Runtime, oferece um conjunto rico de propriedades e métodos para operações de arquivo, ampliando além do que a manipulação de arquivos tradicional do VBA oferece (ex., `Open`, `Print` #, `Write` #). Além de manipular arquivos, o FSO também pode manipular pastas e drives, tornando-o uma ferramenta poderosa para operações de sistema de arquivos dentro do VBA.

Vale ressaltar, contudo, que embora o FSO apresente uma abordagem mais moderna para operações de arquivo em VBA, ele pode introduzir sobrecarga para tarefas simples em comparação com as instruções nativas de manipulação de arquivos do VBA. Além disso, como o FSO faz parte de uma biblioteca externa, portabilidade e compatibilidade com outros sistemas (ex., versões anteriores do Office, Mac Office) podem ser preocupações.

Em contextos onde desempenho, compatibilidade ou dependências externas mínimas são críticas, programadores podem considerar usar as técnicas nativas de manipulação de arquivos do VBA. No entanto, para operações mais complexas ou quando trabalhando em um ambiente onde essas preocupações são mitigadas (como um ambiente corporativo controlado), os benefícios do FileSystemObject muitas vezes superam suas desvantagens.
