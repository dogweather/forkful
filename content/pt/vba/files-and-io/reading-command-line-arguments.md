---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 21:59:12.473652-07:00
description: "Ler argumentos da linha de comando em Visual Basic para Aplica\xE7\xF5\
  es (VBA) envolve acessar par\xE2metros passados ao seu programa durante a execu\xE7\
  \xE3o. Esta\u2026"
lastmod: '2024-03-13T22:44:46.429053-06:00'
model: gpt-4-0125-preview
summary: "Ler argumentos da linha de comando em Visual Basic para Aplica\xE7\xF5es\
  \ (VBA) envolve acessar par\xE2metros passados ao seu programa durante a execu\xE7\
  \xE3o."
title: Lendo argumentos da linha de comando
weight: 23
---

## Como fazer:
Diferentemente de ambientes de programação mais diretos, o VBA não possui um recurso embutido para ler diretamente argumentos da linha de comando em um sentido convencional porque é projetado principalmente para ser incorporado nas aplicações do Microsoft Office. No entanto, com um pouco de criatividade, podemos usar o Windows Script Host (WSH) ou chamar APIs externas para alcançar funcionalidades semelhantes. Aqui está uma solução prática usando o WSH:

1. **Criar um VBScript para Passar Argumentos ao VBA:**

   Primeiro, escreva um arquivo VBScript (*seuScript.vbs*) que inicie sua aplicação VBA (por exemplo, uma macro do Excel) e passe os argumentos da linha de comando:

```vb
Set objExcel = CreateObject("Excel.Application")
objExcel.Workbooks.Open "C:\SeuMacroWorkbook.xlsm"
objExcel.Run "SeuNomeDeMacro", WScript.Arguments.Item(0), WScript.Arguments.Item(1)
objExcel.Quit
```

2. **Acessar os Argumentos no VBA:**

   Na sua aplicação VBA (*SeuMacroWorkbook.xlsm*), modifique ou crie a macro (*SeuNomeDeMacro*) para aceitar parâmetros:

```vb
Sub SeuNomeDeMacro(arg1 As String, arg2 As String)
    MsgBox "Argumento 1: " & arg1 & " Argumento 2: " & arg2
End Sub
```

3. **Execute o Seu Script:**

   Execute o VBScript da linha de comando, passando argumentos conforme necessário:

```shell
cscript seuScript.vbs "Olá" "Mundo"
```

   Isso deve resultar na execução da sua macro VBA com os argumentos "Olá" e "Mundo", exibindo-os em uma caixa de mensagem.

## Aprofundando:
No contexto histórico, o VBA foi concebido para ampliar as capacidades das aplicações do Microsoft Office, não como um ambiente de programação independente. Como tal, a interação direta com a linha de comando está fora de seu escopo principal, o que explica a falta de suporte embutido para a leitura de argumentos da linha de comando.

O método descrito acima, embora eficaz, é mais uma solução alternativa do que uma solução nativa, aproveitando a scriptação externa para preencher a lacuna. Essa abordagem pode introduzir complexidade e preocupações potenciais de segurança, pois requer a habilitação de macros e, potencialmente, a redução de configurações de segurança para execução.

Para tarefas fortemente dependentes de argumentos da linha de comando ou que necessitam de integração mais contínua com o sistema operacional Windows, outras linguagens de programação como PowerShell ou Python podem oferecer soluções mais robustas e seguras. Essas alternativas fornecem suporte direto para argumentos da linha de comando e são mais adequadas para aplicações autônomas ou scripts que requerem entrada externa para modificar seu comportamento dinamicamente.
