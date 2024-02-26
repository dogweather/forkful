---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:56:20.443641-07:00
description: "O tratamento de erros no Visual Basic for Applications (VBA) refere-se\
  \ ao processo de antecipar, detectar e resolver erros de programa\xE7\xE3o, aplicativos\
  \ ou\u2026"
lastmod: '2024-02-25T18:49:44.040470-07:00'
model: gpt-4-0125-preview
summary: "O tratamento de erros no Visual Basic for Applications (VBA) refere-se ao\
  \ processo de antecipar, detectar e resolver erros de programa\xE7\xE3o, aplicativos\
  \ ou\u2026"
title: Gerenciando erros
---

{{< edit_this_page >}}

## O que é & Por que?

O tratamento de erros no Visual Basic for Applications (VBA) refere-se ao processo de antecipar, detectar e resolver erros de programação, aplicativos ou comunicação. Implementar um tratamento de erros robusto é crucial para manter a integridade das aplicações e melhorar a experiência do usuário ao gerenciar problemas inesperados de forma graciosa, sem causar travamentos abruptos ou perda de dados.

## Como fazer:

No VBA, o tratamento de erros é geralmente implementado usando a instrução `On Error`, que instrui o VBA sobre como proceder quando um erro ocorre. As estratégias mais comuns de tratamento de erros envolvem `On Error GoTo` label, `On Error Resume Next` e `On Error GoTo 0`.

**Exemplo 1: Usando `On Error GoTo`**

Esta abordagem permite direcionar o programa para uma seção específica do código, rotulada imediatamente após encontrar um erro.

```vb
Sub ErrorHandlerExample()
    On Error GoTo ErrHandler
    Dim intDivision As Integer

    intDivision = 5 / 0 ' Isso causará um erro de divisão por zero

    Exit Sub
ErrHandler:
    MsgBox "Ocorreu um erro: " & Err.Description, vbCritical, "Erro!"
    Resume Next
End Sub
```

Neste exemplo, qualquer erro em tempo de execução acionará o salto para `ErrHandler`, exibindo uma mensagem de erro e, em seguida, prosseguindo com a próxima linha após o erro.

**Exemplo 2: Usando `On Error Resume Next`**

Esta estratégia instrui o VBA a continuar executando a próxima linha de código mesmo que ocorra um erro, o que pode ser útil para erros considerados inofensivos ou quando se planeja tratar o erro mais tarde na execução.

```vb
Sub ResumeNextExample()
    On Error Resume Next
    Dim intDivision As Integer
    intDivision = 5 / 0 ' Isso não fará o programa parar; erro é ignorado
    
    ' Verificar se ocorreu um erro
    If Err.Number <> 0 Then
        MsgBox "Ocorreu um erro: " & Err.Description, vbExclamation, "Erro Tratado"
        ' Resetar erro
        Err.Clear
    End If
End Sub
```

Neste caso, o programa não para por causa do erro; ele verifica se ocorreu um erro, trata-o se ocorreu, e então limpa o erro.

## Aprofundamento

Historicamente, o tratamento de erros em linguagens de programação evoluiu de simples declarações goto para mecanismos mais sofisticados como exceções em linguagens como Java e C#. O tratamento de erros no VBA, embora não tão poderoso ou flexível quanto o tratamento moderno de exceções, cumpre seu propósito dentro do contexto da aplicação da linguagem em automatizar tarefas em ambientes do Microsoft Office.

A principal limitação do tratamento de erros no VBA reside em sua abordagem um tanto desajeitada e manual, requerendo a colocação cuidadosa do código de tratamento de erros e compreensão clara do fluxo de execução. Linguagens de programação modernas normalmente oferecem soluções mais elegantes, como blocos try-catch, que automaticamente direcionam o fluxo para o código de tratamento de erros sem a necessidade de verificações manuais ou saltos na execução do código.

Apesar dessas limitações, os mecanismos de tratamento de erros do VBA são adequados para a maioria das tarefas de automação e, quando usados corretamente, podem reduzir significativamente a probabilidade de erros não tratados causarem problemas para os usuários. Além disso, entender o tratamento de erros do VBA pode proporcionar insights sobre paradigmas de programação mais antigos e a evolução das estratégias de tratamento de erros no desenvolvimento de software.
